package org.opencb.opencga.storage.mongodb.sequence;

import org.opencb.biodata.formats.io.FileFormatException;
import org.opencb.biodata.formats.sequence.fasta.Fasta;
import org.opencb.biodata.formats.sequence.fasta.dbadaptor.SequenceDBAdaptor;
import org.opencb.biodata.formats.sequence.fasta.io.FastaReader;
import org.opencb.biodata.models.feature.Region;
import org.opencb.opencga.lib.common.XObject;
import org.opencb.opencga.storage.mongodb.utils.SqliteManager;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.SQLException;
import java.util.List;

/**
 * Created by jacobo on 18/08/14.
 */
public class SqliteSequenceDBAdaptor extends SequenceDBAdaptor {

    public static final String SEQUENCE_TABLE = "SEQUENCE";
    public static final String META_TABLE = "META";
    private static final int CHUNK_SIZE = 2000;
    private Path dbPath;
    //private Path fastaPath;
    private SqliteManager sqliteManager;
    private int chunkStart;


    public SqliteSequenceDBAdaptor() {
        sqliteManager = new SqliteManager();
    }
    /**
     *
     * @param input Accept formats: *.properties, *.sqlite.db
     */
    public SqliteSequenceDBAdaptor(Path input) {
        this();
        if(input.toString().endsWith(".fasta") || input.toString().endsWith(".fasta.gz")){
            //createDB(input);
            throw new UnsupportedOperationException("Unimplemented. Needs to call \"this.createDB()\" first."); //TODO: Search db?
        } else if (input.toString().endsWith(".properties")){
            throw new UnsupportedOperationException("Unimplemented");
        } else if(input.toString().endsWith(".sqlite.db")) {
            dbPath = input;
        }
    }

    @Override
    public void open() throws IOException {
        try {
            sqliteManager.connect(dbPath, true);
        } catch (ClassNotFoundException | SQLException e) {
            throw new IOException(e);
        }
    }

    @Override
    public void close() throws IOException {
        try {
            sqliteManager.disconnect(true);
        } catch (SQLException e) {
            throw new IOException(e);
        }
    }


    /**
     * Creates the ChunkId for SQLite
     * @param chromosome  Region name or chromosome
     * @param pos         Absolute position
     * @return            ChunkID. <chromosome>_<pos/CHUNK_SIZE>
     */
    private String getChunkId(String chromosome, int pos){
        return String.format("%s_%06d", chromosome, pos/CHUNK_SIZE);
    }

    @Override
    public String getSequence(Region region) throws IOException {
        /*
         *  A                   B    C
         *  |----|----|----|----|----|      == seq
         *     |-------------------|        == region
         *     D                   E
         *  A : chunkStart * CHUNK_SIZE
         *  B : chunkEnd * CHUNK_SIZE
         *  C : (chunkEnd+1) * CHUNK_SIZE
         *  D : region.getStart()
         *  E : region.getEnd()
         */

        List<XObject> query;
        int chunkStart = region.getStart()/CHUNK_SIZE;
        int chunkEnd = region.getEnd()/CHUNK_SIZE;
        int regionLength = region.getEnd() - region.getStart() + 1;               //+1 to include last position. [start-end]
        if(regionLength <= 0){
            return "";      //Reject bad regions.
        }

        try {
//            query = sqliteManager.query(
//                    "SELECT seq FROM " + SEQUENCE_TABLE +
//                            " WHERE id IS " + region.getChromosome() +
//                            " AND chunk BETWEEN " + chunkStart + " AND " + chunkEnd );
            query = sqliteManager.query(
                    "SELECT seq FROM " + SEQUENCE_TABLE +
                            " WHERE id BETWEEN '" + getChunkId(region.getChromosome(), region.getStart()) + "'"+
                            " AND '" + getChunkId(region.getChromosome(), region.getEnd()) + "'" );
        } catch (SQLException e) {
            throw new IOException(e);
        }

        String seq = "";
        for(XObject xo : query){
            seq += xo.getString("seq");
        }


        int startIndex = region.getStart() - chunkStart * CHUNK_SIZE;        // D - A
        //int endClipping = (chunkEnd+1) * CHUNK_SIZE - region.getEnd();          // C - E
        //int endIndex = seq.length() - endClipping + 1;
        int endIndex = startIndex + regionLength;                          //
        //System.out.println(0 + " - " + startIndex + " - " + endIndex  + " - " + seq.length());
        //System.out.println(chunkStart * CHUNK_SIZE + " - " + region.getStart() + " - " + region.getEnd() + " - " + (chunkEnd+1) * CHUNK_SIZE);
        seq = seq.substring(startIndex, endIndex);

        return seq;
    }

    @Override
    public String getSequence(Region region, String species) throws IOException {
        return getSequence(region);
    }

    /**
     * Creates a <input>.sqlite.db.
     *
     * @param fastaInputFile Accept formats: *.fasta, *.fasta.gz
     */
    public void createDB(Path fastaInputFile) throws IOException, SQLException, FileFormatException {
        Path output = Paths.get(fastaInputFile.toAbsolutePath().toString() + ".sqlite.db");

        try {
            sqliteManager.connect(output, false);
        } catch (ClassNotFoundException e) {
            throw new IOException(e);
        }


        //Create Tables
        XObject seqColumns = new XObject();
        seqColumns.put("id", "TEXT");
        //seqColumns.put("chunk", "INTEGER");
        seqColumns.put("seq", "CHARACTER(" + CHUNK_SIZE + ")");

        XObject metaColumns = new XObject();
        metaColumns.put("id", "TEXT");
        metaColumns.put("description", "TEXT");
        metaColumns.put("length", "INTEGER");

        sqliteManager.createTable(SEQUENCE_TABLE, seqColumns);
        sqliteManager.createTable(META_TABLE, metaColumns);


        FastaReader reader;
        Fasta fasta;


        //Insert Sequences
        reader = new FastaReader(fastaInputFile);
        while((fasta = reader.read()) != null) {
            serializeGenomeSequence(fasta);
        }


        //Create Index
        XObject indices = new XObject();
        indices.put("id", 0);
        //indices.put("chunk", 1);
        sqliteManager.createIndex(SEQUENCE_TABLE, "id", indices);

        dbPath = output;

        sqliteManager.disconnect(true);


    }


    private void serializeGenomeSequence(Fasta fasta) throws SQLException {
        System.out.println(fasta.getDescription());
        System.out.println(fasta.getId());
        System.out.println(fasta.getSeq().length());

        String tablename = SEQUENCE_TABLE;

//        XObject seqColumns = new XObject();
//        seqColumns.put("id", "INTEGER");
//        seqColumns.put("seq", "CHARACTER(2000)");
//        tablename = "SEQ_"+fasta.getId();
//        sqliteManager.createTable(tablename, seqColumns);

        XObject meta = new XObject();
        meta.put("id", fasta.getId());
        meta.put("description", fasta.getDescription());
        meta.put("length", fasta.getSeq().length());

        sqliteManager.insert(meta, META_TABLE);

        int chunks = (fasta.getSeq().length()+CHUNK_SIZE-1)/CHUNK_SIZE; //ceil(length/chunkSize)
        XObject seq = new XObject();
        int end;
        for(int i = 0; i < chunks; i++){
            //seq.put("id", fasta.getId());
            seq.put("id", getChunkId(fasta.getId(), i*CHUNK_SIZE));
            //seq.put("chunk", i);
            end = (i+1)*CHUNK_SIZE;
            if(end >= fasta.getSeq().length()){
                end = fasta.getSeq().length()-1;
            }
            seq.put("seq", fasta.getSeq().substring(i*CHUNK_SIZE, end));
            sqliteManager.insert(seq, tablename);
        }


    }

//    private void parse(Path fastaInputFile){
//        try {
//            String sequenceName = "";
//            String sequenceType = "";
//            String sequenceAssembly = "";
//            String line;
//            StringBuilder sequenceStringBuilder = new StringBuilder();
//            // Preparing input and output files
//            BufferedReader br;
//
//            if(fastaInputFile.toString().endsWith(".gz")) {
//                br = new BufferedReader(new InputStreamReader(new GZIPInputStream(new FileInputStream(fastaInputFile.toFile()))));
//            } else {
//                //br = Files.newBufferedReader(Paths.get(genomeReferenceFastaFile.getAbsolutePath()), Charset.defaultCharset());
//                br = FileUtils.newBufferedReader(fastaInputFile);
//            }
//
//            while ((line = br.readLine()) != null) {
//                if (!line.startsWith(">")) {
//                    sequenceStringBuilder.append(line);
//                } else {
//                    // new chromosome, save data
//                    if (sequenceStringBuilder.length() > 0) {
//                        if(!sequenceName.contains("PATCH") && !sequenceName.contains("HSCHR")) {
//                            System.out.println(sequenceName);
//                            serializeGenomeSequence(sequenceName, sequenceType, sequenceAssembly, sequenceStringBuilder.toString());
//                        }
//                    }
//                    // initialize data structures
//                    sequenceName = line.replace(">", "").split(" ")[0];
//                    sequenceType = line.replace(">", "").split(" ")[2].split(":")[0];
//                    sequenceAssembly = line.replace(">", "").split(" ")[2].split(":")[1];
//                    sequenceStringBuilder.delete(0, sequenceStringBuilder.length());
//                }
//            }
//            // Last chromosome must be processed
//            if(!sequenceName.contains("PATCH") && !sequenceName.contains("HSCHR")) {
//                serializeGenomeSequence(sequenceName, sequenceType, sequenceAssembly, sequenceStringBuilder.toString());
//            }
//            br.close();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//
//    }

//    private void serializeGenomeSequence(String chromosome, String sequenceType, String sequenceAssembly, String sequence){
//        System.out.println(chromosome + " " + sequenceType + " " + sequenceAssembly + "[" + sequence.length() + "]");
//        System.out.println("");
//    }



}