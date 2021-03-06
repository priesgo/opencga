package org.opencb.opencga.catalog.managers;

import org.junit.Before;
import org.junit.Rule;
import org.junit.rules.ExpectedException;
import org.opencb.commons.datastore.core.ObjectMap;
import org.opencb.commons.datastore.core.QueryOptions;
import org.opencb.commons.datastore.core.QueryResult;
import org.opencb.commons.test.GenericTest;
import org.opencb.commons.utils.StringUtils;
import org.opencb.opencga.catalog.db.api.FileDBAdaptor;
import org.opencb.opencga.catalog.exceptions.CatalogException;
import org.opencb.opencga.core.models.*;

import java.io.ByteArrayInputStream;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class AbstractManagerTest extends GenericTest {

    public final static String PASSWORD = "asdf";
    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Rule
    public CatalogManagerExternalResource catalogManagerResource = new CatalogManagerExternalResource();

    protected CatalogManager catalogManager;
    protected String sessionIdUser;
    protected String sessionIdUser2;
    protected String sessionIdUser3;
    protected File testFolder;
    protected String project1;
    protected String project2;
    protected long studyUid;
    protected String studyFqn;
    protected long studyUid2;
    protected String studyFqn2;
    protected String studyFqn3;
    protected String s_1;
    protected String s_2;
    protected String s_3;
    protected String s_4;
    protected String s_5;
    protected String s_6;
    protected String s_7;
    protected String s_8;
    protected String s_9;


    @Before
    public void setUp() throws IOException, CatalogException {
        catalogManager = catalogManagerResource.getCatalogManager();
        setUpCatalogManager(catalogManager);
    }

    public void setUpCatalogManager(CatalogManager catalogManager) throws IOException, CatalogException {

        catalogManager.getUserManager().create("user", "User Name", "mail@ebi.ac.uk", PASSWORD, "", null, Account.FULL, null, null);
        catalogManager.getUserManager().create("user2", "User2 Name", "mail2@ebi.ac.uk", PASSWORD, "", null, Account.FULL, null, null);
        catalogManager.getUserManager().create("user3", "User3 Name", "user.2@e.mail", PASSWORD, "ACME", null, Account.FULL, null, null);

        sessionIdUser = catalogManager.getUserManager().login("user", PASSWORD);
        sessionIdUser2 = catalogManager.getUserManager().login("user2", PASSWORD);
        sessionIdUser3 = catalogManager.getUserManager().login("user3", PASSWORD);

        project1 = catalogManager.getProjectManager().create("1000G", "Project about some genomes", "", "ACME", "Homo sapiens",
                null, null, "GRCh38", new QueryOptions(), sessionIdUser).first().getId();
        project2 = catalogManager.getProjectManager().create("pmp", "Project Management Project", "life art intelligent system",
                "myorg", "Homo sapiens", null, null, "GRCh38", new QueryOptions(), sessionIdUser2).first().getId();
        catalogManager.getProjectManager().create("p1", "project 1", "", "", "Homo sapiens", null, null, "GRCh38", new QueryOptions(),
                sessionIdUser3).first();

        Study study = catalogManager.getStudyManager().create(project1, "phase1", null, "Phase 1", Study.Type.TRIO, null, "Done", null, null, null, null, null, null, null, null, sessionIdUser).first();
        studyUid = study.getUid();
        studyFqn = study.getFqn();

        study = catalogManager.getStudyManager().create(project1, "phase3", null, "Phase 3", Study.Type.CASE_CONTROL, null, "d", null, null, null, null, null, null, null, null, sessionIdUser).first();
        studyUid2 = study.getUid();
        studyFqn2 = study.getFqn();

        study = catalogManager.getStudyManager().create(project2, "s1", null, "Study 1", Study.Type.CONTROL_SET, null, "", null, null, null, null, null, null, null, null, sessionIdUser2).first();
        studyFqn3 = study.getFqn();

        catalogManager.getFileManager().createFolder(studyFqn2, Paths.get("data/test/folder/").toString(), null, true,
                null, QueryOptions.empty(), sessionIdUser);

        testFolder = catalogManager.getFileManager().createFolder(studyFqn, Paths.get("data/test/folder/").toString(),
                null, true, null, QueryOptions.empty(), sessionIdUser).first();
        ObjectMap attributes = new ObjectMap();
        attributes.put("field", "value");
        attributes.put("numValue", 5);
        catalogManager.getFileManager().update(studyFqn, testFolder.getPath(), new ObjectMap("attributes", attributes), new QueryOptions(),
                sessionIdUser);

        QueryResult<File> queryResult2 = catalogManager.getFileManager().create(studyFqn,
                new File().setPath(testFolder.getPath() + "test_1K.txt.gz"), false, StringUtils.randomString(1000), null, sessionIdUser);

        File fileTest1k = catalogManager.getFileManager().get(studyFqn, queryResult2.first().getPath(), null, sessionIdUser).first();
        attributes = new ObjectMap();
        attributes.put("field", "value");
        attributes.put("name", "fileTest1k");
        attributes.put("numValue", "10");
        attributes.put("boolean", false);
        catalogManager.getFileManager().update(studyFqn, fileTest1k.getPath(), new ObjectMap("attributes", attributes), new QueryOptions(),
                sessionIdUser);

        QueryResult<File> queryResult1 = catalogManager.getFileManager().create(studyFqn,
                new File().setPath(testFolder.getPath() + "test_0.5K.txt").setBioformat(File.Bioformat.DATAMATRIX_EXPRESSION), false,
                StringUtils.randomString(500), null, sessionIdUser);

        File fileTest05k = catalogManager.getFileManager().get(studyFqn, queryResult1.first().getPath(), null, sessionIdUser).first();
        attributes = new ObjectMap();
        attributes.put("field", "valuable");
        attributes.put("name", "fileTest05k");
        attributes.put("numValue", 5);
        attributes.put("boolean", true);
        catalogManager.getFileManager().update(studyFqn, fileTest05k.getPath(), new ObjectMap("attributes", attributes), new QueryOptions(),
                sessionIdUser);

        QueryResult<File> queryResult = catalogManager.getFileManager().create(studyFqn,
                new File().setPath(testFolder.getPath() + "test_0.1K.png").setFormat(File.Format.IMAGE), false,
                StringUtils.randomString(100), null, sessionIdUser);

        File test01k = catalogManager.getFileManager().get(studyFqn, queryResult.first().getPath(), null, sessionIdUser).first();
        attributes = new ObjectMap();
        attributes.put("field", "other");
        attributes.put("name", "test01k");
        attributes.put("numValue", 50);
        attributes.put("nested", new ObjectMap("num1", 45).append("num2", 33).append("text", "HelloWorld"));
        catalogManager.getFileManager().update(studyFqn, test01k.getPath(), new ObjectMap("attributes", attributes), new QueryOptions(),
                sessionIdUser);

        List<Variable> variables = new ArrayList<>();
        variables.addAll(Arrays.asList(
                new Variable("NAME", "", "", Variable.VariableType.TEXT, "", true, false, Collections.<String>emptyList(), 0, "", "", null,
                        Collections.<String, Object>emptyMap()),
                new Variable("AGE", "", "", Variable.VariableType.INTEGER, null, true, false, Collections.singletonList("0:130"), 1, "", "",
                        null, Collections.<String, Object>emptyMap()),
                new Variable("HEIGHT", "", "", Variable.VariableType.DOUBLE, "1.5", false, false, Collections.singletonList("0:"), 2, "",
                        "", null, Collections.<String, Object>emptyMap()),
                new Variable("ALIVE", "", "", Variable.VariableType.BOOLEAN, "", true, false, Collections.<String>emptyList(), 3, "", "",
                        null, Collections.<String, Object>emptyMap()),
                new Variable("PHEN", "", "", Variable.VariableType.CATEGORICAL, "CASE", true, false, Arrays.asList("CASE", "CONTROL"), 4,
                        "", "", null, Collections.<String, Object>emptyMap()),
                new Variable("EXTRA", "", "", Variable.VariableType.TEXT, "", false, false, Collections.emptyList(), 5, "", "", null,
                        Collections.<String, Object>emptyMap())
        ));
        VariableSet vs = catalogManager.getStudyManager().createVariableSet(studyFqn, "vs", "vs", true, false, "", null, variables,
                sessionIdUser).first();

        Sample sample = new Sample().setId("s_1");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot1", vs.getId(),
                new ObjectMap("NAME", "s_1").append("AGE", 6).append("ALIVE", true).append("PHEN", "CONTROL"))));
        s_1 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_2");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot1", vs.getId(),
                new ObjectMap("NAME", "s_2").append("AGE", 10).append("ALIVE", false).append("PHEN", "CASE"))));
        s_2 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_3");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot1", vs.getId(),
                new ObjectMap("NAME", "s_3").append("AGE", 15).append("ALIVE", true).append("PHEN", "CONTROL"))));
        s_3 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_4");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot1", vs.getId(),
                new ObjectMap("NAME", "s_4").append("AGE", 22).append("ALIVE", false).append("PHEN", "CONTROL"))));
        s_4 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_5");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot1", vs.getId(),
                new ObjectMap("NAME", "s_5").append("AGE", 29).append("ALIVE", true).append("PHEN", "CASE"))));
        s_5 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_6");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot2", vs.getId(),
                new ObjectMap("NAME", "s_6").append("AGE", 38).append("ALIVE", true).append("PHEN", "CONTROL"))));
        s_6 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_7");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot2", vs.getId(),
                new ObjectMap("NAME", "s_7").append("AGE", 46).append("ALIVE", false).append("PHEN", "CASE"))));
        s_7 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_8");
        sample.setAnnotationSets(Collections.singletonList(new AnnotationSet("annot2", vs.getId(),
                new ObjectMap("NAME", "s_8").append("AGE", 72).append("ALIVE", true).append("PHEN", "CONTROL"))));
        s_8 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        sample.setId("s_9");
        sample.setAnnotationSets(Collections.emptyList());
        s_9 = catalogManager.getSampleManager().create(studyFqn, sample, new QueryOptions(), sessionIdUser).first().getId();

        catalogManager.getFileManager().update(studyFqn, test01k.getPath(),
                new ObjectMap(FileDBAdaptor.QueryParams.SAMPLES.key(), Arrays.asList(s_1, s_2, s_3, s_4, s_5)), new QueryOptions(),
                sessionIdUser);
    }


    /* TYPE_FILE UTILS */
    public static java.io.File createDebugFile() throws IOException {
        String fileTestName = "/tmp/fileTest_" + StringUtils.randomString(5);
        return createDebugFile(fileTestName);
    }

    public static java.io.File createDebugFile(String fileTestName) throws IOException {
        return createDebugFile(fileTestName, 200);
    }

    public static java.io.File createDebugFile(String fileTestName, int lines) throws IOException {
        DataOutputStream os = new DataOutputStream(new FileOutputStream(fileTestName));

        os.writeBytes("Debug file name: " + fileTestName + "\n");
        for (int i = 0; i < 100; i++) {
            os.writeBytes(i + ", ");
        }
        for (int i = 0; i < lines; i++) {
            os.writeBytes(StringUtils.randomString(500));
            os.write('\n');
        }
        os.close();

        return Paths.get(fileTestName).toFile();
    }

}
