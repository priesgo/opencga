package org.opencb.opencga.storage.core.metadata;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.junit.Before;
import org.junit.Test;
import org.opencb.biodata.models.variant.VariantFileMetadata;
import org.opencb.biodata.models.variant.metadata.VariantMetadata;
import org.opencb.opencga.storage.core.variant.VariantStorageBaseTest;
import org.opencb.opencga.storage.core.variant.io.VariantReaderUtils;

import java.io.IOException;
import java.net.URI;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;

/**
 * Created on 09/08/17.
 *
 * @author Jacobo Coll &lt;jacobo167@gmail.com&gt;
 */
public class VariantMetadataConverterTest {


    private StudyConfiguration studyConfiguration;
    private VariantMetadataConverter variantMetadataConverter;
    private ObjectWriter objectWriter;
    private ProjectMetadata projectMetadata;

    @Before
    public void setUp() throws Exception {
        studyConfiguration = new StudyConfiguration(1, "study");
        studyConfiguration.getSampleIds().put("s1", 1);
        studyConfiguration.getSampleIds().put("s2", 2);
        studyConfiguration.getSampleIds().put("s3", 3);
        studyConfiguration.getSampleIds().put("s4", 4);
        studyConfiguration.getSampleIds().put("s5", 5);
        studyConfiguration.getSampleIds().put("s6", 6);
        studyConfiguration.getSampleIds().put("s7", 7);

        studyConfiguration.getIndexedFiles().add(10);
        studyConfiguration.getFileIds().put("file1.vcf", 10);
        studyConfiguration.getSamplesInFiles().put(10, new LinkedHashSet<>(Arrays.asList(1, 2, 3, 4)));

        studyConfiguration.getIndexedFiles().add(11);
        studyConfiguration.getFileIds().put("file2.vcf", 11);
        studyConfiguration.getSamplesInFiles().put(11, new LinkedHashSet<>(Arrays.asList(4, 5, 6)));

        studyConfiguration.getCalculatedStats().add(20);
        studyConfiguration.getCohortIds().put("ALL", 20);
        studyConfiguration.getCohorts().put(20, new HashSet<>(Arrays.asList(1, 2, 3, 4, 5, 6)));

        URI uri = VariantStorageBaseTest.getResourceUri("platinum/1K.end.platinum-genomes-vcf-NA12877_S1.genome.vcf.gz");
        VariantFileMetadata fileMetadata = VariantReaderUtils.readVariantFileMetadata(Paths.get(uri), null);
        studyConfiguration.addVariantFileHeader(fileMetadata.getHeader(), null);

        variantMetadataConverter = new VariantMetadataConverter();
        objectWriter = new ObjectMapper()
                .configure(MapperFeature.REQUIRE_SETTERS_FOR_GETTERS, true)
                .setSerializationInclusion(JsonInclude.Include.NON_EMPTY)
                .writerWithDefaultPrettyPrinter();
        projectMetadata = new ProjectMetadata("hsapiens", "37", 1);
    }

    @Test
    public void toVariantMetadataTest() throws IOException {

        VariantMetadata variantMetadata = variantMetadataConverter.toVariantMetadata(Collections.singletonList(studyConfiguration), projectMetadata, null, null);
        System.out.println("variantMetadata = " + objectWriter.writeValueAsString(variantMetadata));

    }

}