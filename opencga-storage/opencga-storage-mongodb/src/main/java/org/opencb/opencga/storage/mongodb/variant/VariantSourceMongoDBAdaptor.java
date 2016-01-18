package org.opencb.opencga.storage.mongodb.variant;

import com.mongodb.BasicDBObject;
import com.mongodb.DBObject;
import com.mongodb.QueryBuilder;
import java.net.UnknownHostException;
import java.util.*;

import com.mongodb.WriteResult;
import org.opencb.biodata.models.variant.VariantSource;
import org.opencb.biodata.models.variant.stats.VariantGlobalStats;
import org.opencb.biodata.models.variant.stats.VariantSourceStats;
import org.opencb.datastore.core.QueryOptions;
import org.opencb.datastore.core.QueryResult;
import org.opencb.datastore.mongodb.MongoDBCollection;
import org.opencb.datastore.mongodb.MongoDBConfiguration;
import org.opencb.datastore.mongodb.MongoDataStore;
import org.opencb.datastore.mongodb.MongoDataStoreManager;
import org.opencb.opencga.storage.mongodb.utils.MongoCredentials;
import org.opencb.opencga.storage.core.variant.adaptors.VariantSourceDBAdaptor;

/**
 *
 * @author Cristina Yenyxe Gonzalez Garcia <cyenyxe@ebi.ac.uk>
 */
public class VariantSourceMongoDBAdaptor implements VariantSourceDBAdaptor {

    private static final Map<String, List> samplesInSources = new HashMap<>();
    
    private final MongoDataStoreManager mongoManager;
    private final MongoDataStore db;
    private final DBObjectToVariantSourceConverter variantSourceConverter;
    private final String collectionName;

    
    public VariantSourceMongoDBAdaptor(MongoCredentials credentials, String collectionName) throws UnknownHostException {
        // Mongo configuration
        mongoManager = new MongoDataStoreManager(credentials.getDataStoreServerAddresses());
        MongoDBConfiguration mongoDBConfiguration = credentials.getMongoDBConfiguration();
        db = mongoManager.get(credentials.getMongoDbName(), mongoDBConfiguration);
        this.collectionName = collectionName;
        variantSourceConverter = new DBObjectToVariantSourceConverter();
    }

    @Override
    public QueryResult countSources() {
        MongoDBCollection coll = db.getCollection(collectionName);
        return coll.count();
    }

    @Override
    public QueryResult<VariantSource> getAllSources(QueryOptions options) {
        MongoDBCollection coll = db.getCollection(collectionName);
        QueryBuilder qb = QueryBuilder.start();
        parseQueryOptions(options, qb);
        
        return coll.find(qb.get(), null, variantSourceConverter, options);
    }

    @Override
    public QueryResult getAllSourcesByStudyId(String studyId, QueryOptions options) {
        return getAllSourcesByStudyIds(Collections.singletonList(studyId), options);
    }

    @Override
    public QueryResult getAllSourcesByStudyIds(List<String> studyIds, QueryOptions options) {
        MongoDBCollection coll = db.getCollection(collectionName);
        QueryBuilder qb = QueryBuilder.start();
//        getStudyIdFilter(studyIds, qb);
        options.put("studyId", studyIds);
        parseQueryOptions(options, qb);
        
        return coll.find(qb.get(), null, variantSourceConverter, options);
    }

    @Override
    public QueryResult getSamplesBySource(String fileId, QueryOptions options) {    // TODO jmmut: deprecate when we remove fileId, and change for getSamplesBySource(String studyId, QueryOptions options)
        return getSamplesBySources(Collections.singletonList(fileId), options);
    }
    
    @Override
    public QueryResult getSamplesBySources(List<String> fileIds, QueryOptions options) {
        // if any of the fileIds is not present in the "samplesInSources" cache, retrieve them again
        if (!isAllSourcesCached(fileIds)) {
            synchronized (StudyMongoDBAdaptor.class) {
                if (!isAllSourcesCached(fileIds)) {
                    QueryResult queryResult = populateSamplesInSources();
                    populateSamplesQueryResult(fileIds, queryResult);
                    return queryResult;
                }
            }
        }

        QueryResult queryResult = new QueryResult();
        populateSamplesQueryResult(fileIds, queryResult);
        return queryResult;
    }

    private boolean isAllSourcesCached(List<String> fileIds) {
        boolean available = true;

        for (String fileId : fileIds) {
            if (!samplesInSources.containsKey(fileId)) {
                available = false;
                break;
            }
        }
        return available;
    }

    @Override
    public QueryResult getSourceDownloadUrlByName(String filename) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public List<QueryResult> getSourceDownloadUrlByName(List<String> filenames) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public QueryResult getSourceDownloadUrlById(String fileId, String studyId) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    @Override
    public boolean close() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private void parseQueryOptions(QueryOptions options, QueryBuilder builder) {

        if(options.containsKey("studyId")) {
            andIs(DBObjectToVariantSourceConverter.STUDYID_FIELD, options.get("studyId"), builder);
        }
        if(options.containsKey("studyName")) {
            andIs(DBObjectToVariantSourceConverter.STUDYNAME_FIELD, options.get("studyId"), builder);
        }
        if(options.containsKey("fileId")) {
            andIs(DBObjectToVariantSourceConverter.FILEID_FIELD, options.get("fileId"), builder);
        }
        if(options.containsKey("fileName")) {
            andIs(DBObjectToVariantSourceConverter.FILENAME_FIELD, options.get("fileName"), builder);
        }


    }

    private QueryBuilder andIs(String fieldName, Object object, QueryBuilder builder) {
        if(object == null) {
            return builder;
        } else if (object instanceof Collection) {
            return builder.and(fieldName).in(object);
        } else {
            return builder.and(fieldName).is(object);
        }
    }
//
//    private QueryBuilder getStudyIdFilter(String id, QueryBuilder builder) {
//        return builder.and(DBObjectToVariantSourceConverter.STUDYID_FIELD).is(id);
//    }
//
//    private QueryBuilder getStudyIdFilter(List<String> ids, QueryBuilder builder) {
//        return builder.and(DBObjectToVariantSourceConverter.STUDYID_FIELD).in(ids);
//    }
    
    /**
     * Repopulates the dictionary relating sources and samples.
     * 
     * @return The QueryResult with information of how long the query took
     */
    private QueryResult populateSamplesInSources() {
        MongoDBCollection coll = db.getCollection(collectionName);
        DBObject projection = new BasicDBObject(DBObjectToVariantSourceConverter.FILEID_FIELD, true)
                .append(DBObjectToVariantSourceConverter.SAMPLES_FIELD, true);
        QueryResult queryResult = coll.find((DBObject)null, projection, null);
        
        List<DBObject> result = queryResult.getResult();
        samplesInSources.clear();
        for (DBObject dbo : result) {
            String key = dbo.get(DBObjectToVariantSourceConverter.FILEID_FIELD).toString();
            DBObject value = (DBObject) dbo.get(DBObjectToVariantSourceConverter.SAMPLES_FIELD);

            // replace '£' by '.'
            List<String> sampleNames = new ArrayList<>();
            for (String mongoSampleName : value.keySet()) {
                sampleNames.add(mongoSampleName.replace(DBObjectToVariantSourceConverter.CHARACTER_TO_REPLACE_DOTS, '.'));
            }
            samplesInSources.put(key, sampleNames);
        }
        
        return queryResult;
    }

    /*
    private void populateSamplesQueryResult(String fileId, QueryResult queryResult) {
        List<List> samples = new ArrayList<>(1);
        List<String> samplesInSource = samplesInSources.get(fileId);

        if (samplesInSource == null || samplesInSource.isEmpty()) {
            queryResult.setWarningMsg("Source " + fileId + " not found");
            queryResult.setNumTotalResults(0);
        } else {
            samples.add(samplesInSource);
            queryResult.setResult(samples);
            queryResult.setNumTotalResults(1);
        }
    }
    */

    private void populateSamplesQueryResult(List<String> fileIds, QueryResult queryResult) {
        List<List> samples = new ArrayList<>(fileIds.size());
        
        for (String fileId : fileIds) {
            List<String> samplesInSource = samplesInSources.get(fileId);

            if (samplesInSource == null || samplesInSource.isEmpty()) {
                // Samples not found
                samples.add(new ArrayList<>());
                if (queryResult.getWarningMsg() == null) {
                    queryResult.setWarningMsg("Source " + fileId + " not found");
                } else {
                    queryResult.setWarningMsg(queryResult.getWarningMsg().concat("\nSource " + fileId + " not found"));
                }
//                queryResult.setNumTotalResults(0);
            } else {
                // Add new list of samples
                samples.add(samplesInSource);
//                queryResult.setNumTotalResults(1);
            }
        }
        
        queryResult.setResult(samples);
        queryResult.setNumTotalResults(fileIds.size());
    }


    @Override
    public QueryResult updateSourceStats(VariantSourceStats variantSourceStats, QueryOptions queryOptions) {
        MongoDBCollection coll = db.getCollection(collectionName);

        VariantGlobalStats global = variantSourceStats.getFileStats();
        DBObject globalStats = new BasicDBObject(DBObjectToVariantSourceConverter.NUMSAMPLES_FIELD, global.getSamplesCount())
                .append(DBObjectToVariantSourceConverter.NUMVARIANTS_FIELD, global.getVariantsCount())
                .append(DBObjectToVariantSourceConverter.NUMSNPS_FIELD, global.getSnpsCount())
                .append(DBObjectToVariantSourceConverter.NUMINDELS_FIELD, global.getIndelsCount())
                .append(DBObjectToVariantSourceConverter.NUMPASSFILTERS_FIELD, global.getPassCount())
                .append(DBObjectToVariantSourceConverter.NUMTRANSITIONS_FIELD, global.getTransitionsCount())
                .append(DBObjectToVariantSourceConverter.NUMTRANSVERSIONS_FIELD, global.getTransversionsCount())
                .append(DBObjectToVariantSourceConverter.MEANQUALITY_FIELD, (double) global.getMeanQuality());

        DBObject find = new BasicDBObject(DBObjectToVariantSourceConverter.FILEID_FIELD, variantSourceStats.getFileId())
                .append(DBObjectToVariantSourceConverter.STUDYID_FIELD, variantSourceStats.getStudyId());
        DBObject update = new BasicDBObject("$set", new BasicDBObject(DBObjectToVariantSourceConverter.STATS_FIELD, globalStats));

        return coll.update(find, update, null);
    }


}
