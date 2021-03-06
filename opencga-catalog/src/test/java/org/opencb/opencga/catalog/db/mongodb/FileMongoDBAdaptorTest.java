/*
 * Copyright 2015-2017 OpenCB
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.opencb.opencga.catalog.db.mongodb;

import org.bson.Document;
import org.junit.Ignore;
import org.junit.Test;
import org.opencb.commons.datastore.core.ObjectMap;
import org.opencb.commons.datastore.core.Query;
import org.opencb.commons.datastore.core.QueryOptions;
import org.opencb.commons.datastore.core.QueryResult;
import org.opencb.commons.utils.StringUtils;
import org.opencb.opencga.catalog.db.api.FileDBAdaptor;
import org.opencb.opencga.catalog.exceptions.CatalogDBException;
import org.opencb.opencga.catalog.exceptions.CatalogException;
import org.opencb.opencga.core.models.File;
import org.opencb.opencga.core.models.Job;
import org.opencb.opencga.core.models.Sample;
import org.opencb.opencga.core.models.acls.permissions.FileAclEntry;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

/**
 * Created by pfurio on 3/2/16.
 */
public class FileMongoDBAdaptorTest extends MongoDBAdaptorTest {

    @Test
    public void createFileToStudyTest() throws CatalogException {
        long studyId = user3.getProjects().get(0).getStudies().get(0).getUid();
        assertTrue(studyId >= 0);
        File file;
        file = new File("jobs/", File.Type.DIRECTORY, File.Format.PLAIN, File.Bioformat.NONE, "jobs/", "",
                new File.FileStatus(File.FileStatus.STAGE), 1000, 1);
        LinkedList<FileAclEntry> acl = new LinkedList<>();
        acl.push(new FileAclEntry("jcoll", Arrays.asList(FileAclEntry.FilePermissions.VIEW.name(),
                FileAclEntry.FilePermissions.VIEW_CONTENT.name(), FileAclEntry.FilePermissions.VIEW_HEADER.name(),
                FileAclEntry.FilePermissions.DELETE.name())));
        acl.push(new FileAclEntry("jmmut", Collections.emptyList()));
        System.out.println(catalogFileDBAdaptor.insert(file, studyId, null));
        file = new File("file.sam", File.Type.FILE, File.Format.PLAIN, File.Bioformat.ALIGNMENT, "data/file.sam", "",
                new File.FileStatus(File.FileStatus.STAGE), 1000, 1);
        System.out.println(catalogFileDBAdaptor.insert(file, studyId, null));
        file = new File("file.bam", File.Type.FILE, File.Format.BINARY, File.Bioformat.ALIGNMENT, "data/file.bam", "",
                new File.FileStatus(File.FileStatus.STAGE), 1000, 1);
        System.out.println(catalogFileDBAdaptor.insert(file, studyId, null));
        file = new File("file.vcf", File.Type.FILE, File.Format.PLAIN, File.Bioformat.VARIANT, "data/file2.vcf", "",
                new File.FileStatus(File.FileStatus.STAGE), 1000, 1);

        try {
            System.out.println(catalogFileDBAdaptor.insert(file, -20, null));
            fail("Expected \"StudyId not found\" exception");
        } catch (CatalogDBException e) {
            System.out.println(e);
        }

        System.out.println(catalogFileDBAdaptor.insert(file, studyId, null));

        try {
            System.out.println(catalogFileDBAdaptor.insert(file, studyId, null));
            fail("Expected \"File already exist\" exception");
        } catch (CatalogDBException e) {
            System.out.println(e);
        }
    }

    @Test
    public void getFileTest() throws CatalogDBException {
        File file = user3.getProjects().get(0).getStudies().get(0).getFiles().get(0);
        QueryResult<File> fileQueryResult = catalogFileDBAdaptor.get(file.getUid(), null);
        System.out.println(fileQueryResult);
        try {
            System.out.println(catalogFileDBAdaptor.get(-1, null));
            fail("Expected \"FileId not found\" exception");
        } catch (CatalogDBException e) {
            System.out.println(e);
        }
    }

    @Test
    public void getAllFilesStudyNotValidTest() throws CatalogDBException {
        thrown.expect(CatalogDBException.class);
        thrown.expectMessage("not valid");
        catalogFileDBAdaptor.getAllInStudy(-1, null);
    }

    @Test
    public void getAllFilesStudyNotExistsTest() throws CatalogDBException {
        thrown.expect(CatalogDBException.class);
        thrown.expectMessage("not exist");
        catalogFileDBAdaptor.getAllInStudy(216544, null);
    }

    @Test
    public void getAllFilesTest() throws CatalogDBException {
        long studyId = user3.getProjects().get(0).getStudies().get(0).getUid();
        QueryResult<File> allFiles = catalogFileDBAdaptor.getAllInStudy(studyId, null);
        List<File> files = allFiles.getResult();
        List<File> expectedFiles = user3.getProjects().get(0).getStudies().get(0).getFiles();
        assertEquals(expectedFiles.size(), files.size());
        for (File expectedFile : expectedFiles) {
            boolean found = false;
            for (File fileResult : allFiles.getResult()) {
                if (fileResult.getUid() == expectedFile.getUid())
                    found = true;
            }
            if (!found) {
                throw new CatalogDBException("The file " + expectedFile.getName() + " could not be found.");
            }
        }
    }

//    // Test if the lookup operation works fine
//    @Test
//    public void getFileWithJob() throws CatalogDBException {
//        long studyId = user3.getProjects().get(0).getStudies().get(0).getUid();
//        QueryOptions queryOptions = new QueryOptions();
//
//        // We create a job
//        String jobName = "jobName";
//        String jobDescription = "This is the description of the job";
//        Job myJob = new Job().setName(jobName).setDescription(jobDescription);
//        QueryResult<Job> jobInsert = catalogJobDBAdaptor.insert(myJob, studyId, queryOptions);
//
//        // We create a new file giving that job
//        File file = new File().setName("Filename").setPath("data/Filename").setJob(jobInsert.first());
//        QueryResult<File> fileInsert = catalogFileDBAdaptor.insert(file, studyId, queryOptions);
//
//        // Get the file
//        QueryResult<File> noJobInfoQueryResult = catalogFileDBAdaptor.get(fileInsert.first().getUid(), queryOptions);
//        assertNull(noJobInfoQueryResult.first().getJob().getName());
//        assertNull(noJobInfoQueryResult.first().getJob().getDescription());
//
//        queryOptions.put("lazy", false);
//        QueryResult<File> jobInfoQueryResult = catalogFileDBAdaptor.get(fileInsert.first().getUid(), queryOptions);
//        assertEquals(jobName, jobInfoQueryResult.first().getJob().getName());
//        assertEquals(jobDescription, jobInfoQueryResult.first().getJob().getDescription());
//    }

    @Test
    public void modifyFileTest() throws CatalogDBException, IOException {
        File file = user3.getProjects().get(0).getStudies().get(0).getFiles().get(0);
        long fileId = file.getUid();

        Document stats = new Document("stat1", 1).append("stat2", true).append("stat3", "ok" + StringUtils.randomString(20));

        ObjectMap parameters = new ObjectMap();
        parameters.put("status.name", File.FileStatus.READY);
        parameters.put("stats", stats);
        System.out.println(catalogFileDBAdaptor.update(fileId, parameters, QueryOptions.empty()));

        file = catalogFileDBAdaptor.get(fileId, null).first();
        assertEquals(file.getStatus().getName(), File.FileStatus.READY);
        assertEquals(file.getStats(), stats);

        parameters = new ObjectMap();
        parameters.put("stats", "{}");
        System.out.println(catalogFileDBAdaptor.update(fileId, parameters, QueryOptions.empty()));

        file = catalogFileDBAdaptor.get(fileId, null).first();
        assertEquals(file.getStats(), new LinkedHashMap<String, Object>());
    }

    @Test
    public void renameFileTest() throws CatalogDBException {
        String newName = "newFile.bam";
        String parentPath = "data/";
        long fileId = catalogFileDBAdaptor.getId(user3.getProjects().get(0).getStudies().get(0).getUid(), "data/file.vcf");
        System.out.println(catalogFileDBAdaptor.rename(fileId, parentPath + newName, "", null));

        File file = catalogFileDBAdaptor.get(fileId, null).first();
        assertEquals(file.getName(), newName);
        assertEquals(file.getPath(), parentPath + newName);

        try {
            catalogFileDBAdaptor.rename(-1, "noFile", "", null);
            fail("error: expected \"file not found\"exception");
        } catch (CatalogDBException e) {
            System.out.println("correct exception: " + e);
        }

        long folderId = catalogFileDBAdaptor.getId(user3.getProjects().get(0).getStudies().get(0).getUid(), "data/");
        String folderName = "folderName";
        catalogFileDBAdaptor.rename(folderId, folderName, "", null);
        assertTrue(catalogFileDBAdaptor.get(fileId, null).first().getPath().equals(folderName + "/" + newName));
    }

    @Test
    public void includeFields() throws CatalogDBException {
        QueryResult<File> fileQueryResult = catalogFileDBAdaptor.get(7,
                new QueryOptions(QueryOptions.INCLUDE, FileDBAdaptor.QueryParams.PATH.key()));
        List<File> files = fileQueryResult.getResult();
        assertEquals("Include path does not work.", "data/file.vcf", files.get(0).getPath());
        assertEquals("Include not working.", null, files.get(0).getName());
    }

    @Test
    public void testDistinct() throws Exception {

//        List<String> distinctOwners = catalogFileDBAdaptor.distinct(new Query(), CatalogFileDBAdaptor.QueryParams.OWNER_ID.key()).getResult();
        List<String> distinctTypes = catalogFileDBAdaptor.distinct(new Query(), FileDBAdaptor.QueryParams.TYPE.key()).getResult();
//        assertEquals(Arrays.asList("imedina", "pfurio"), distinctOwners);
        assertEquals(Arrays.asList("DIRECTORY","FILE"), distinctTypes);

        List<Long> pfurioStudies = Arrays.asList(9L, 14L);
        List<String> distinctFormats = catalogFileDBAdaptor.distinct(
                new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), pfurioStudies),
                FileDBAdaptor.QueryParams.FORMAT.key()).getResult();
        assertEquals(Arrays.asList("UNKNOWN", "COMMA_SEPARATED_VALUES", "BAM"), distinctFormats);

        distinctFormats = catalogFileDBAdaptor.distinct(new Query(),
                FileDBAdaptor.QueryParams.FORMAT.key()).getResult();
        Collections.sort(distinctFormats);
        List<String> expected = Arrays.asList("PLAIN", "UNKNOWN", "COMMA_SEPARATED_VALUES", "BAM");
        Collections.sort(expected);
        assertEquals(expected, distinctFormats);
    }

    @Test
    public void testRank() throws Exception {
        List<Long> pfurioStudies = Arrays.asList(9L, 14L);
        List<Document> rankedFilesPerDiskUsage = catalogFileDBAdaptor.rank(
                new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), pfurioStudies),
                FileDBAdaptor.QueryParams.SIZE.key(), 100, false).getResult();

        assertEquals(3, rankedFilesPerDiskUsage.size());

        assertEquals(100, rankedFilesPerDiskUsage.get(0).get("_id"));
        assertEquals(3, rankedFilesPerDiskUsage.get(0).get("count"));

        assertEquals(5000, rankedFilesPerDiskUsage.get(1).get("_id"));
        assertEquals(2, rankedFilesPerDiskUsage.get(1).get("count"));

        assertEquals(10, rankedFilesPerDiskUsage.get(2).get("_id"));
        assertEquals(2, rankedFilesPerDiskUsage.get(2).get("count"));
    }

    @Test
    public void testGroupBy() throws Exception {
        List<Long> pfurioStudies = Arrays.asList(9L, 14L);

        List<Document> groupByBioformat = catalogFileDBAdaptor.groupBy(new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), pfurioStudies),
                FileDBAdaptor.QueryParams.BIOFORMAT.key(), new QueryOptions()).getResult();

        assertEquals("ALIGNMENT", ((Document) groupByBioformat.get(0).get("_id")).get(FileDBAdaptor.QueryParams.BIOFORMAT.key()));
        assertEquals(Arrays.asList("m_alignment.bam", "alignment.bam"), groupByBioformat.get(0).get("items"));

        assertEquals("NONE", ((Document) groupByBioformat.get(1).get("_id")).get(FileDBAdaptor.QueryParams.BIOFORMAT.key()));
        assertTrue(Arrays.asList("m_file1.txt", "file2.txt", "file1.txt", "data/")
                .containsAll((Collection<?>) groupByBioformat.get(1).get("items")));

        groupByBioformat = catalogFileDBAdaptor.groupBy(new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), 14), // MINECO study
                FileDBAdaptor.QueryParams.BIOFORMAT.key(), new QueryOptions()).getResult();

        assertEquals("ALIGNMENT", ((Document) groupByBioformat.get(0).get("_id")).get(FileDBAdaptor.QueryParams.BIOFORMAT.key()));
        assertEquals(Arrays.asList("m_alignment.bam"), groupByBioformat.get(0).get("items"));

        assertEquals("NONE", ((Document) groupByBioformat.get(1).get("_id")).get(FileDBAdaptor.QueryParams.BIOFORMAT.key()));
        assertEquals(Arrays.asList("m_file1.txt", "data/"), groupByBioformat.get(1).get("items"));

    }

    @Test
    public void testGroupBy1() throws Exception {

        List<Long> pfurioStudies = Arrays.asList(9L, 14L);
        List<Document> groupByBioformat = catalogFileDBAdaptor.groupBy(
                new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), pfurioStudies),
                Arrays.asList(FileDBAdaptor.QueryParams.BIOFORMAT.key(), FileDBAdaptor.QueryParams.TYPE.key()),
                new QueryOptions()).getResult();

        assertEquals(3, groupByBioformat.size());

        assertEquals(2, ((Document) groupByBioformat.get(0).get("_id")).size()); // Alignment - File
        assertEquals(Arrays.asList("m_alignment.bam", "alignment.bam"), groupByBioformat.get(0).get("items"));

        assertEquals(2, ((Document) groupByBioformat.get(1).get("_id")).size()); // None - File
        assertEquals(Arrays.asList("m_file1.txt", "file2.txt", "file1.txt"), groupByBioformat.get(1).get("items"));

        assertEquals(2, ((Document) groupByBioformat.get(2).get("_id")).size()); // None - Folder
        assertEquals(Arrays.asList("data/"), groupByBioformat.get(2).get("items"));

    }

    @Test
    public void testAddSamples() throws Exception {
        long studyUid = user3.getProjects().get(0).getStudies().get(0).getUid();
        Sample sample1 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample1"), QueryOptions.empty())
                .first();
        Sample sample2 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample2"), QueryOptions.empty())
                .first();

        File file = user3.getProjects().get(0).getStudies().get(0).getFiles().get(0);
        catalogFileDBAdaptor.addSamplesToFile(file.getUid(),
                Arrays.asList(sample1, sample2));

        QueryResult<File> fileQueryResult = catalogFileDBAdaptor.get(file.getUid(), QueryOptions.empty());
        assertEquals(2, fileQueryResult.first().getSamples().size());
        assertTrue(Arrays.asList(sample1.getUid(), sample2.getUid()).containsAll(
                fileQueryResult.first().getSamples().stream().map(Sample::getUid).collect(Collectors.toList())));

        Sample sample3 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample3"), QueryOptions.empty())
                .first();
        // Test we avoid duplicities
        catalogFileDBAdaptor.addSamplesToFile(file.getUid(),
                Arrays.asList(sample1, sample2, sample2, sample3));
        fileQueryResult = catalogFileDBAdaptor.get(file.getUid(), QueryOptions.empty());
        assertEquals(3, fileQueryResult.first().getSamples().size());
        assertTrue(Arrays.asList(sample1.getUid(), sample2.getUid(), sample3.getUid()).containsAll(
                fileQueryResult.first().getSamples().stream().map(Sample::getUid).collect(Collectors.toList())));
    }

    @Test
    public void testRemoveSamples() throws Exception {
        long studyUid = user3.getProjects().get(0).getStudies().get(0).getUid();
        Sample sample1 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample1"), QueryOptions.empty())
                .first();
        Sample sample2 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample2"), QueryOptions.empty())
                .first();
        Sample sample3 = catalogDBAdaptor.getCatalogSampleDBAdaptor().insert(studyUid, new Sample().setId("sample3"), QueryOptions.empty())
                .first();
        File file = user3.getProjects().get(0).getStudies().get(0).getFiles().get(0);
        catalogFileDBAdaptor.addSamplesToFile(file.getUid(), Arrays.asList(sample1, sample2, sample3));

        QueryResult<File> fileQueryResult = catalogFileDBAdaptor.get(file.getUid(), QueryOptions.empty());
        assertEquals(3, fileQueryResult.first().getSamples().size());
        assertTrue(Arrays.asList(sample1.getUid(), sample2.getUid(), sample3.getUid())
                .containsAll(fileQueryResult.first().getSamples().stream().map(Sample::getUid).collect(Collectors.toList())));

        catalogFileDBAdaptor.extractSampleFromFiles(new Query(FileDBAdaptor.QueryParams.UID.key(), file.getUid()),
                Arrays.asList(sample1.getUid(), sample3.getUid()));
        fileQueryResult = catalogFileDBAdaptor.get(file.getUid(), QueryOptions.empty());
        assertEquals(1, fileQueryResult.first().getSamples().size());
        assertTrue(fileQueryResult.first().getSamples().get(0).getUid() == sample2.getUid());
    }

    @Test
    public void testGroupByDates() throws Exception {
        List<Long> pfurioStudies = Arrays.asList(9L, 14L);

        List<Document> groupByBioformat = catalogFileDBAdaptor.groupBy(
                new Query(FileDBAdaptor.QueryParams.STUDY_UID.key(), pfurioStudies),
                Arrays.asList(FileDBAdaptor.QueryParams.BIOFORMAT.key(), FileDBAdaptor.QueryParams.TYPE.key(), "day"),
                new QueryOptions()).getResult();

        assertEquals(3, groupByBioformat.size());

        for (int i = 0; i < groupByBioformat.size(); i++) {
            String bioformat = ((Document) groupByBioformat.get(i).get("_id")).getString("bioformat");
            String type = ((Document) groupByBioformat.get(i).get("_id")).getString("type");
            switch (bioformat) {
                case "NONE":
                    switch (type) {
                        case "FILE":
                            assertEquals(5, ((Document) groupByBioformat.get(i).get("_id")).size()); // None - File
                            assertEquals(Arrays.asList("m_file1.txt", "file2.txt", "file1.txt"), groupByBioformat.get(i).get("items"));
                            break;
                        default:
                            assertEquals(5, ((Document) groupByBioformat.get(i).get("_id")).size()); // None - Folder
                            assertEquals(Arrays.asList("data/"), groupByBioformat.get(i).get("items"));
                            break;
                    }
                    break;
                case "ALIGNMENT":
                    assertEquals(5, ((Document) groupByBioformat.get(i).get("_id")).size());
                    assertEquals(Arrays.asList("m_alignment.bam", "alignment.bam"), groupByBioformat.get(i).get("items"));
                    break;
                default:
                    fail("This case should not happen.");
                    break;
            }
        }
    }
}
