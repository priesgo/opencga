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

package org.opencb.opencga.server.rest;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.*;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.glassfish.jersey.media.multipart.FormDataContentDisposition;
import org.glassfish.jersey.media.multipart.FormDataParam;
import org.opencb.commons.datastore.core.*;
import org.opencb.opencga.catalog.db.api.FileDBAdaptor;
import org.opencb.opencga.catalog.exceptions.CatalogException;
import org.opencb.opencga.catalog.exceptions.CatalogIOException;
import org.opencb.opencga.catalog.io.CatalogIOManager;
import org.opencb.opencga.catalog.managers.AbstractManager;
import org.opencb.opencga.catalog.managers.FileManager;
import org.opencb.opencga.catalog.managers.FileUtils;
import org.opencb.opencga.catalog.utils.Constants;
import org.opencb.opencga.catalog.utils.FileMetadataReader;
import org.opencb.opencga.catalog.utils.FileScanner;
import org.opencb.opencga.catalog.utils.ParamUtils;
import org.opencb.opencga.core.common.IOUtils;
import org.opencb.opencga.core.common.UriUtils;
import org.opencb.opencga.core.exception.VersionException;
import org.opencb.opencga.core.models.*;
import org.opencb.opencga.core.models.acls.AclParams;
import org.opencb.opencga.core.models.acls.permissions.FileAclEntry;
import org.opencb.opencga.core.models.acls.permissions.StudyAclEntry;
import org.opencb.opencga.server.rest.json.mixin.FileMixin;
import org.opencb.opencga.storage.core.manager.variant.VariantStorageManager;
import org.opencb.opencga.storage.core.variant.adaptors.VariantQueryParam;
import org.opencb.opencga.storage.core.variant.annotation.VariantAnnotationManager;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.*;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.*;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;

import static org.opencb.opencga.storage.core.variant.VariantStorageEngine.Options.*;


@Path("/{apiVersion}/files")
@Produces(MediaType.APPLICATION_JSON)
@Api(value = "Files", position = 4, description = "Methods for working with 'files' endpoint")
public class FileWSServer extends OpenCGAWSServer {

    private FileManager fileManager;

    public FileWSServer(@Context UriInfo uriInfo, @Context HttpServletRequest httpServletRequest, @Context HttpHeaders httpHeaders)
            throws IOException, VersionException {
        super(uriInfo, httpServletRequest, httpHeaders);
        fileManager = catalogManager.getFileManager();
    }

    @POST
    @Path("/create")
    @Consumes(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Create file or folder", response = File[].class,
            notes = "Creates a file with some content in it or a folder <br>"
                    + "<ul>"
                    + "<il><b>path</b>: Mandatory parameter. Whole path containing the file or folder name to be created</il><br>"
                    + "<il><b>content</b>: Content of the file. Only applicable if <b>directory</b> parameter set to false</il><br>"
                    + "<il><b>description</b>: Description of the file or folder to store as metadata.</il><br>"
                    + "<il><b>parents</b>: Create the parent directories if they do not exist.</il><br>"
                    + "<il><b>directory</b>: Boolean indicating whether to create a file or a directory</il><br>"
                    + "<ul>"
    )
    public Response createFilePOST(@ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                                   @QueryParam("study") String studyStr,
                                   @ApiParam(name = "params", value = "File parameters", required = true) FileCreateParams params) {
        try {
            ObjectUtils.defaultIfNull(params, new FileCreateParams());
            QueryResult<File> file;
            if (params.directory) {
                // Create directory
                file = fileManager.createFolder(studyStr, params.path, new File.FileStatus(File.FileStatus.READY), params.parents,
                        params.description, queryOptions, sessionId);
            } else {
                // Create a file
                file = fileManager.createFile(studyStr, params.path, params.description, params.parents, params.content, sessionId);
            }
            return createOkResponse(file);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{files}/info")
    @ApiOperation(value = "File info", position = 3, response = File[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided",
                    example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided",
                    example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "lazy", value = "False to return entire job and experiment object", defaultValue = "true",
                    dataType = "boolean", paramType = "query"),
            @ApiImplicitParam(name = Constants.FLATTENED_ANNOTATIONS, value = "Flatten the annotations?", defaultValue = "false",
                    dataType = "boolean", paramType = "query")
    })
    public Response info(@ApiParam(value = "Comma separated list of file ids or names up to a maximum of 100")
                         @PathParam(value = "files") String fileStr,
                         @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                         @QueryParam("study") String studyStr,
                         @ApiParam(value = "Boolean to accept either only complete (false) or partial (true) results", defaultValue = "false") @QueryParam("silent") boolean silent) {
        try {
            query.remove("study");
            query.remove("files");

            List<String> idList = getIdList(fileStr);
            List<QueryResult<File>> fileQueryResult = fileManager.get(studyStr, idList, query, queryOptions, silent, sessionId);
            return createOkResponse(fileQueryResult);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/bioformats")
    @ApiOperation(value = "List of accepted file bioformats", position = 3)
    public Response getBioformats() {
        List<File.Bioformat> bioformats = Arrays.asList(File.Bioformat.values());
        QueryResult<File.Bioformat> queryResult = new QueryResult("Bioformats", 0, bioformats.size(), bioformats.size(), "", "", bioformats);
        return createOkResponse(queryResult);
    }

    @GET
    @Path("/formats")
    @ApiOperation(value = "List of accepted file formats", position = 3)
    public Response getFormats() {
        List<File.Format> formats = Arrays.asList(File.Format.values());
        QueryResult<File.Format> queryResult = new QueryResult("Formats", 0, formats.size(), formats.size(), "", "", formats);
        return createOkResponse(queryResult);
    }

    @POST
    @Path("/upload")
    @Consumes(MediaType.MULTIPART_FORM_DATA)
    @ApiOperation(httpMethod = "POST", position = 4, value = "Resource to upload a file by chunks", response = File.class)
    public Response upload(
            @ApiParam(hidden = true) @FormDataParam("chunk_content") byte[] chunkBytes,
            @ApiParam(hidden = true) @FormDataParam("chunk_content") FormDataContentDisposition contentDisposition,
            @FormDataParam("file") InputStream fileInputStream,
            @FormDataParam("file") FormDataContentDisposition fileMetaData,

            @ApiParam(hidden = true) @DefaultValue("") @FormDataParam("chunk_id") String chunk_id,
            @ApiParam(hidden = true) @DefaultValue("false") @FormDataParam("last_chunk") String last_chunk,
            @ApiParam(hidden = true) @DefaultValue("") @FormDataParam("chunk_total") String chunk_total,
            @ApiParam(hidden = true) @DefaultValue("") @FormDataParam("chunk_size") String chunk_size,
            @ApiParam(hidden = true) @DefaultValue("") @FormDataParam("chunk_hash") String chunkHash,
            @ApiParam(hidden = true) @DefaultValue("false") @FormDataParam("resume_upload") String resume_upload,

            @ApiParam(value = "filename", required = false) @FormDataParam("filename") String filename,
            @ApiParam(value = "fileFormat", required = true) @DefaultValue("") @FormDataParam("fileFormat") File.Format fileFormat,
            @ApiParam(value = "bioformat", required = true) @DefaultValue("") @FormDataParam("bioformat") File.Bioformat bioformat,
            @ApiParam(value = "(DEPRECATED) Use study instead", hidden = true) @FormDataParam("studyId") String studyIdStr,
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias") @FormDataParam("study") String studyStr,
            @ApiParam(value = "Path within catalog where the file will be located (default: root folder)",
                    required = true) @DefaultValue(".") @FormDataParam("relativeFilePath") String relativeFilePath,
            @ApiParam(value = "description", required = false) @DefaultValue("") @FormDataParam("description")
                    String description,
            @ApiParam(value = "Create the parent directories if they do not exist", required = false) @DefaultValue("true") @FormDataParam("parents") boolean parents) {

        if (StringUtils.isNotEmpty(studyIdStr)) {
            studyStr = studyIdStr;
        }

        long t = System.currentTimeMillis();

        if (!relativeFilePath.endsWith("/")) {
            relativeFilePath = relativeFilePath + "/";
        }

        if (relativeFilePath.startsWith("/")) {
            return createErrorResponse(new CatalogException("The path cannot be absolute"));
        }

        java.nio.file.Path filePath;
        final Study study;
        try {
            String userId = catalogManager.getUserManager().getUserId(sessionId);
            study = catalogManager.getStudyManager().resolveId(studyStr, userId);
            catalogManager.getAuthorizationManager().checkStudyPermission(study.getUid(), userId,
                    StudyAclEntry.StudyPermissions.UPLOAD_FILES);
            // TODO: Improve upload method. Check upload permission not only at study level.
        } catch (Exception e) {
            return createErrorResponse(e);
        }

        try {
            filePath = Paths.get(catalogManager.getFileManager().getUri(study.getUid(), relativeFilePath));
        } catch (CatalogException e) {
            return createErrorResponse(e);
        }

        if (chunkBytes != null && filePath != null) {

            java.nio.file.Path completedFilePath = filePath.getParent().resolve("_" + filename);
            java.nio.file.Path folderPath = filePath.getParent().resolve("__" + filename);

            logger.info(relativeFilePath + "");
            logger.info(folderPath + "");
            logger.info(filePath + "");
            boolean resume = Boolean.parseBoolean(resume_upload);

            try {
                logger.info("---resume is: " + resume);
                if (resume) {
                    logger.info("Resume ms :" + (System.currentTimeMillis() - t));
                    return createOkResponse(getResumeFileJSON(folderPath));
                }

                int chunkId = Integer.parseInt(chunk_id);
                int chunkSize = Integer.parseInt(chunk_size);
                boolean lastChunk = Boolean.parseBoolean(last_chunk);

                logger.info("---saving chunk: " + chunkId);
                logger.info("lastChunk: " + lastChunk);

                // WRITE CHUNK TYPE_FILE
                if (!Files.exists(folderPath)) {
                    logger.info("createDirectory(): " + folderPath);
                    Files.createDirectory(folderPath);
                }
                logger.info("check dir " + Files.exists(folderPath));
                // String hash = StringUtils.sha1(new String(chunkBytes));
                // logger.info("bytesHash: " + hash);
                // logger.info("chunkHash: " + chunkHash);
                // hash = chunkHash;
                if (chunkBytes.length == chunkSize) {
                    Files.write(folderPath.resolve(chunkId + "_" + chunkBytes.length + "_partial"), chunkBytes);
                } else {
                    String errorMessage = "Chunk content size (" + chunkBytes.length + ") " +
                            "!= chunk_size (" + chunk_size + ").";
                    logger.error(errorMessage);
                    return createErrorResponse(new IOException(errorMessage));
                }

                if (lastChunk) {
                    logger.info("lastChunk is true...");
                    Files.deleteIfExists(completedFilePath);
                    Files.createFile(completedFilePath);
                    List<java.nio.file.Path> chunks = getSortedChunkList(folderPath);
                    logger.info("----ordered chunks length: " + chunks.size());
                    for (java.nio.file.Path partPath : chunks) {
                        logger.info(partPath.getFileName().toString());
                        Files.write(completedFilePath, Files.readAllBytes(partPath), StandardOpenOption.APPEND);
                    }
                    IOUtils.deleteDirectory(folderPath);
                    try {
                        QueryResult<File> queryResult1 = catalogManager.getFileManager().create(studyStr, File.Type.FILE,
                                fileFormat, bioformat, relativeFilePath, null, description, new File.FileStatus(File.FileStatus.STAGE), 0, -1, null, -1, null, null, parents, null, null, sessionId);
                        new FileUtils(catalogManager).upload(completedFilePath.toUri(), queryResult1.first(), null, sessionId, false, false, true, true, Long.MAX_VALUE);
                        QueryResult<File> queryResult = catalogManager.getFileManager().get(queryResult1.first().getUid(), null, sessionId);
                        File file = new FileMetadataReader(catalogManager).setMetadataInformation(queryResult.first(), null,
                                new QueryOptions(queryOptions), sessionId, false);
                        queryResult.setResult(Collections.singletonList(file));
                        return createOkResponse(queryResult);
                    } catch (Exception e) {
                        logger.error(e.toString());
                        return createErrorResponse(e);
                    }
                }
            } catch (IOException e) {
                System.out.println("e = " + e);
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            logger.info("chunk saved ms :" + (System.currentTimeMillis() - t));
            return createOkResponse("ok");

        } else if (fileInputStream != null) {
            if (filename == null) {
                filename = fileMetaData.getFileName();
            }

            File file = new File()
                    .setName(filename)
                    .setPath(relativeFilePath + filename)
                    .setFormat(fileFormat)
                    .setBioformat(bioformat);
            try {
                return createOkResponse(fileManager.upload(studyStr, fileInputStream, file, false, parents, sessionId));
            } catch (Exception e) {
                return createErrorResponse("Upload file", e.getMessage());
            }
        } else {
            return createErrorResponse("Upload file", "No file or chunk found");
        }
    }

    @GET
    @Path("/{file}/download")
    @ApiOperation(value = "Download file", position = 5, response = QueryResponse.class,
            notes = "The usage of /{file}/download webservice through Swagger is <b>discouraged</b>. Please, don't click the 'Try it "
                    + "out' button here as it may hang this web page. Instead, build the final URL in a different tab.<br>"
                    + "An special <b>DOWNLOAD</b> permission is needed to download files from OpenCGA.")
    public Response download(@ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam("file") String fileIdStr,
                             @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                             @QueryParam("study") String studyStr) {
        try {
            isSingleId(fileIdStr);
            DataInputStream stream = catalogManager.getFileManager().download(studyStr, fileIdStr, -1, -1, null, sessionId);
            return createOkResponse(stream, MediaType.APPLICATION_OCTET_STREAM_TYPE, fileIdStr);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{file}/content")
    @ApiOperation(value = "Show the content of a file (up to a limit)", position = 6, response = String.class)
    public Response content(@ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam("file") String fileIdStr,
                            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                            @QueryParam("study") String studyStr,
                            @ApiParam(value = "start", required = false) @QueryParam("start") @DefaultValue("-1") int start,
                            @ApiParam(value = "limit", required = false) @QueryParam("limit") @DefaultValue("-1") int limit) {
        try {
            isSingleId(fileIdStr);
            AbstractManager.MyResource resource = fileManager.getUid(fileIdStr, studyStr, sessionId);
            catalogManager.getAuthorizationManager().checkFilePermission(resource.getStudy().getUid(), resource.getResource().getUid(),
                    resource.getUser(), FileAclEntry.FilePermissions.VIEW_CONTENT);

            DataInputStream stream = catalogManager.getFileManager().download(studyStr, fileIdStr, start, limit, null, sessionId);
            return createOkResponse(stream, MediaType.TEXT_PLAIN_TYPE);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{file}/grep")
    @ApiOperation(value = "Filter lines of the file containing a match of the pattern [NOT TESTED]", position = 7, response = String.class)
    public Response downloadGrep(
            @ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam("file") String fileIdStr,
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
            @QueryParam("study") String studyStr,
            @ApiParam(value = "Pattern", required = false) @QueryParam("pattern") @DefaultValue(".*") String pattern,
            @ApiParam(value = "Do a case insensitive search", required = false) @DefaultValue("false") @QueryParam("ignoreCase")
                    Boolean ignoreCase,
            @ApiParam(value = "Return multiple matches", required = false) @DefaultValue("true") @QueryParam("multi") Boolean multi) {
        try {
            isSingleId(fileIdStr);
            AbstractManager.MyResource<File> resource = fileManager.getUid(fileIdStr, studyStr, sessionId);
            catalogManager.getAuthorizationManager().checkFilePermission(resource.getStudy().getUid(), resource.getResource().getUid(),
                    resource.getUser(), FileAclEntry.FilePermissions.VIEW_CONTENT);

            QueryOptions options = new QueryOptions("ignoreCase", ignoreCase);
            options.put("multi", multi);
            DataInputStream stream = catalogManager.getFileManager().grep(studyStr, fileIdStr, pattern, options, sessionId);
            return createOkResponse(stream, MediaType.TEXT_PLAIN_TYPE);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @Deprecated
    @GET
    @Path("/{file}/set-header")
    @ApiOperation(value = "Set file header [DEPRECATED]", position = 10, notes = "Deprecated method. Moved to update.", hidden = true)
    public Response setHeader(@PathParam(value = "file") @FormDataParam("fileId") String fileStr,
                              @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                              @QueryParam("study") String studyStr,
                              @ApiParam(value = "header", required = true) @DefaultValue("") @QueryParam("header") String header) {
        String content = "";
        DataInputStream stream;
        QueryResult<File> fileQueryResult;
        InputStream streamBody = null;
        try {
            isSingleId(fileStr);
            AbstractManager.MyResource<File> resource = fileManager.getUid(fileStr, studyStr, sessionId);
            catalogManager.getAuthorizationManager().checkFilePermission(resource.getStudy().getUid(), resource.getResource().getUid(),
                    resource.getUser(), FileAclEntry.FilePermissions.WRITE);

            /** Obtain file uri **/
            File file = resource.getResource();
            URI fileUri = catalogManager.getFileManager().getUri(file);
            System.out.println("getUri: " + fileUri.getPath());

            /** Set header **/
            stream = catalogManager.getFileManager().download(studyStr, fileStr, -1, -1, null, sessionId);
            content = org.apache.commons.io.IOUtils.toString(stream);
            String lines[] = content.split(System.getProperty("line.separator"));
            StringBuilder body = new StringBuilder();
            body.append(header);
            body.append(System.getProperty("line.separator"));
            for (int i = 0; i < lines.length; i++) {
                String line = lines[i];
                if (!line.startsWith("#")) {
                    body.append(line);
                    if (i != lines.length - 1)
                        body.append(System.getProperty("line.separator"));
                }
            }
            /** Write/Copy  file **/
            streamBody = new ByteArrayInputStream(body.toString().getBytes(StandardCharsets.UTF_8));
            Files.copy(streamBody, Paths.get(fileUri), StandardCopyOption.REPLACE_EXISTING);

        } catch (Exception e) {
            return createErrorResponse(e);
        }
//        createOkResponse(content, MediaType.TEXT_PLAIN)
        return createOkResponse(streamBody, MediaType.TEXT_PLAIN_TYPE);
    }

    @Deprecated
    @GET
    @Path("/{folder}/files")
    @ApiOperation(value = "File content [DEPRECATED]", position = 11, notes = "Deprecated method. Moved to /list.", hidden = true)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided",
                    example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided",
                    example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "Number of results to be returned in the queries", dataType = "integer",
                    paramType = "query"),
            @ApiImplicitParam(name = "skip", value = "Number of results to skip in the queries", dataType = "integer", paramType = "query")
    })
    public Response getAllFilesInFolder(@PathParam(value = "folder") @FormDataParam("folderId") String folderStr,
                                        @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or "
                                                + "alias") @QueryParam("study") String studyStr) {
        QueryResult<File> results;
        try {
            results = catalogManager.getFileManager().getFilesFromFolder(folderStr, studyStr, queryOptions, sessionId);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
        return createOkResponse(results);
    }

    @GET
    @Path("/search")
    @ApiOperation(value = "File search method.", position = 12, response = File[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided", example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided", example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "Number of results to be returned in the queries", dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = "skip", value = "Number of results to skip in the queries", dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = "count", value = "Total number of results", defaultValue = "false", dataType = "boolean", paramType = "query"),
            @ApiImplicitParam(name = "lazy", value = "False to return entire job and experiment object", defaultValue = "true",
                    dataType = "boolean", paramType = "query"),
            @ApiImplicitParam(name = Constants.FLATTENED_ANNOTATIONS, value = "Flatten the annotations?", defaultValue = "false",
                    dataType = "boolean", paramType = "query")
    })
    public Response search(
            @ApiParam(value = "(DEPRECATED) Use study instead", hidden = true) @QueryParam("studyId")
                    String studyIdStr,
            @ApiParam(value = "Study [[user@]project:]{study}  where study and project can be either the id or alias.")
            @QueryParam("study") String studyStr,
            @ApiParam(value = "Comma separated list of file names") @DefaultValue("") @QueryParam("name") String name,
            @ApiParam(value = "Comma separated list of paths", required = false) @DefaultValue("") @QueryParam("path") String path,
            @ApiParam(value = "Available types (FILE, DIRECTORY)", required = false) @DefaultValue("") @QueryParam("type") String type,
            @ApiParam(value = "Comma separated Bioformat values. For existing Bioformats see files/bioformats", required = false) @DefaultValue("") @QueryParam("bioformat") String bioformat,
            @ApiParam(value = "Comma separated Format values. For existing Formats see files/formats", required = false) @DefaultValue("") @QueryParam("format") String formats,
            @ApiParam(value = "Status", required = false) @DefaultValue("") @QueryParam("status") String status,
            @ApiParam(value = "Directory under which we want to look for files or folders", required = false) @DefaultValue("") @QueryParam("directory") String directory,
            @ApiParam(value = "Creation date (Format: yyyyMMddHHmmss)") @QueryParam("creationDate") String creationDate,
            @ApiParam(value = "Modification date (Format: yyyyMMddHHmmss)", required = false) @DefaultValue("") @QueryParam("modificationDate") String modificationDate,
            @ApiParam(value = "Description", required = false) @DefaultValue("") @QueryParam("description") String description,
            @ApiParam(value = "Tags") @QueryParam("tags") String tags,
            @ApiParam(value = "Size", required = false) @DefaultValue("") @QueryParam("size") String size,
            @ApiParam(value = "Comma separated list of sample ids", hidden = true) @QueryParam("sample") String sample,
            @ApiParam(value = "Comma separated list of sample ids") @QueryParam("samples") String samples,
            @ApiParam(value = "(DEPRECATED) Job id that created the file(s) or folder(s)", hidden = true) @QueryParam("jobId") String jobIdOld,
            @ApiParam(value = "Job id that created the file(s) or folder(s)", required = false) @QueryParam("job.id") String jobId,
            @ApiParam(value = "Annotation, e.g: key1=value(;key2=value)") @QueryParam("annotation") String annotation,
            @ApiParam(value = "Text attributes (Format: sex=male,age>20 ...)", required = false) @DefaultValue("") @QueryParam("attributes") String attributes,
            @ApiParam(value = "Numerical attributes (Format: sex=male,age>20 ...)", required = false) @DefaultValue("")
            @QueryParam("nattributes") String nattributes,
            @ApiParam(value = "Skip count", defaultValue = "false") @QueryParam("skipCount") boolean skipCount,
            @ApiParam(value = "Release value") @QueryParam("release") String release) {
        try {
            query.remove("study");
            queryOptions.put(QueryOptions.SKIP_COUNT, skipCount);

            if (StringUtils.isNotEmpty(studyIdStr)) {
                studyStr = studyIdStr;
            }

            if (StringUtils.isNotEmpty(sample) && !query.containsKey(FileDBAdaptor.QueryParams.SAMPLES.key())) {
                query.put(FileDBAdaptor.QueryParams.SAMPLES.key(), sample);
            }

            if (query.containsKey(FileDBAdaptor.QueryParams.NAME.key())
                    && (query.get(FileDBAdaptor.QueryParams.NAME.key()) == null
                    || query.getString(FileDBAdaptor.QueryParams.NAME.key()).isEmpty())) {
                query.remove(FileDBAdaptor.QueryParams.NAME.key());
                logger.debug("Name attribute empty, it's been removed");
            }
            // TODO: jobId is deprecated. Remember to remove this if after next release
            if (query.containsKey("jobId") && !query.containsKey(FileDBAdaptor.QueryParams.JOB_UID.key())) {
                query.put(FileDBAdaptor.QueryParams.JOB_UID.key(), query.get("jobId"));
                query.remove("jobId");
            }

            QueryResult<File> result;
            if (count) {
                result = fileManager.count(studyStr, query, sessionId);
            } else {
                result = fileManager.search(studyStr, query, queryOptions, sessionId);
            }

            return createOkResponse(result);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{folder}/list")
    @ApiOperation(value = "List all the files inside the folder", position = 13, response = File[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided", example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided", example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "Number of results to be returned in the queries", dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = "skip", value = "Number of results to skip in the queries", dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = "count", value = "Total number of results", defaultValue = "false", dataType = "boolean",
                    paramType = "query")
    })
    public Response list(@ApiParam(value = "Folder id, name or path") @PathParam("folder") String folder,
                         @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                         @QueryParam("study") String studyStr) {
        try {
            isSingleId(folder);
            QueryResult<File> result = catalogManager.getFileManager().getFilesFromFolder(folder, studyStr, queryOptions, sessionId);
            return createOkResponse(result);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @Deprecated
    @GET
    @Path("/{file}/index")
    @ApiOperation(value = "Index variant files [DEPRECATED]", position = 14, notes = "Moved to analysis/[variant|alignment]/{file}/index",
            hidden = true, response = QueryResponse.class)
    public Response index(@ApiParam("Comma separated list of file ids (files or directories)") @PathParam(value = "file") String fileIdStr,
                          @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                          @QueryParam("study") String studyStr,
                          // Study id is not ingested by the analysis index command line. No longer needed.
//                          @ApiParam("Study id") @QueryParam("studyId") String studyId,
                          @ApiParam("Output directory id") @QueryParam("outDir") String outDirStr,
                          @ApiParam("Boolean indicating that only the transform step will be run") @DefaultValue("false")
                          @QueryParam("transform") boolean transform,
                          @ApiParam("Boolean indicating that only the load step will be run") @DefaultValue("false")
                          @QueryParam("load") boolean load,
                          @ApiParam("Comma separated list of fields to be include in the index")
                          @QueryParam("includeExtraFields") String includeExtraFields,
                          @ApiParam("Type of aggregated VCF file: none, basic, EVS or ExAC") @DefaultValue("none")
                          @QueryParam("aggregated") String aggregated,
                          @ApiParam("Calculate indexed variants statistics after the load step") @DefaultValue("false")
                          @QueryParam("calculateStats") boolean calculateStats,
                          @ApiParam("Annotate indexed variants after the load step") @DefaultValue("false")
                          @QueryParam("annotate") boolean annotate,
                          @ApiParam("Overwrite annotations already present in variants") @DefaultValue("false")
                          @QueryParam("overwrite") boolean overwriteAnnotations) {

        Map<String, String> params = new LinkedHashMap<>();
//        addParamIfNotNull(params, "studyId", studyId);
        addParamIfNotNull(params, "outdir", outDirStr);
        addParamIfTrue(params, "transform", transform);
        addParamIfTrue(params, "load", load);
        addParamIfNotNull(params, EXTRA_GENOTYPE_FIELDS.key(), includeExtraFields);
        addParamIfNotNull(params, AGGREGATED_TYPE.key(), aggregated);
        addParamIfTrue(params, CALCULATE_STATS.key(), calculateStats);
        addParamIfTrue(params, ANNOTATE.key(), annotate);
        addParamIfTrue(params, VariantAnnotationManager.OVERWRITE_ANNOTATIONS, overwriteAnnotations);

        Set<String> knownParams = new HashSet<>();
        knownParams.add("outDir");
        knownParams.add("transform");
        knownParams.add("load");
        knownParams.add("includeExtraFields");
        knownParams.add("aggregated");
        knownParams.add("calculateStats");
        knownParams.add("annotate");
        knownParams.add("overwrite");
        knownParams.add("sid");
        knownParams.add("include");
        knownParams.add("exclude");

        // Add other params
        query.forEach((key, value) -> {
            if (!knownParams.contains(key)) {
                if (value != null) {
                    params.put(key, value.toString());
                }
            }
        });

        logger.info("ObjectMap: {}", params);

        try {
            List<String> idList = getIdList(fileIdStr);
            QueryResult queryResult = fileManager.index(studyStr, idList, "VCF", params, sessionId);
            return createOkResponse(queryResult);
        } catch (Exception e) {
            return createErrorResponse(e);
        }


//        AnalysisFileIndexer analysisFileIndexer = new AnalysisFileIndexer(catalogManager);
//
//        try {
//            long outDirId = catalogManager.getFileId(outDirStr, sessionId);
//            long fileId = catalogManager.getFileId(fileIdStr, sessionId);
//            if(outDirId < 0) {
//                outDirId = catalogManager.getFileParent(fileId, null, sessionId).first().getId();
//            }
//            // TODO: Change it to query
//            queryOptions.add(VariantStorageEngine.Options.CALCULATE_STATS.key(), calculateStats);
//            queryOptions.add(VariantStorageEngine.Options.ANNOTATE.key(), annotate);
//            QueryResult<Job> queryResult = analysisFileIndexer.index(fileId, outDirId, sessionId, new QueryOptions(queryOptions));
//            return createOkResponse(queryResult);
//        } catch (Exception e) {
//            return createErrorResponse(e);
//        }
    }

    @GET
    @Path("/{folder}/tree")
    @ApiOperation(value = "Obtain a tree view of the files and folders within a folder", position = 15, response = FileTree[].class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided", example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided", example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "[TO BE IMPLEMENTED] Number of results to be returned in the queries", dataType = "integer", paramType = "query"),
    })
    public Response treeView(@ApiParam(value = "Folder id, name or path. Paths must be separated by : instead of /") @DefaultValue(":")
                             @PathParam("folder") String folderId,
                             @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                             @QueryParam("study") String studyStr,
                             @ApiParam(value = "Maximum depth to get files from") @DefaultValue("5") @QueryParam("maxDepth") int maxDepth) {
        try {
            query.remove("study");
            query.remove("folder");
            query.remove("maxDepth");

            isSingleId(folderId);
            query.remove("maxDepth");
            QueryResult result = fileManager
                    .getTree(folderId.replace(":", "/"), studyStr, query, queryOptions, maxDepth, sessionId);
            return createOkResponse(result);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @Deprecated
    @GET
    @Path("/{file}/variants")
    @ApiOperation(value = "Fetch variants from a VCF/gVCF file [DEPRECATED]", position = 15,
            notes = "Moved to analysis/variants/query", hidden = true, response = QueryResponse.class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided",
                    example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided", example = "id,status", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "Number of results to be returned in the queries", dataType = "integer", paramType = "query"),
            @ApiImplicitParam(name = "skip", value = "Number of results to skip in the queries", dataType = "integer", paramType = "query"),
//            @ApiImplicitParam(name = "count", value = "Total number of results", dataType = "boolean", paramType = "query")
    })
    public Response getVariants(@ApiParam(value = "", required = true) @PathParam("file") String fileIdCsv,
                                @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                                @QueryParam("study") String studyStr,
                                @ApiParam(value = "List of variant ids") @QueryParam("ids") String ids,
                                @ApiParam(value = "List of regions: {chr}:{start}-{end}") @QueryParam("region") String region,
                                @ApiParam(value = "List of chromosomes") @QueryParam("chromosome") String chromosome,
                                @ApiParam(value = "List of genes") @QueryParam("gene") String gene,
                                @ApiParam(value = "Variant type: [SNV, MNV, INDEL, SV, CNV]") @QueryParam("type") String type,
                                @ApiParam(value = "Reference allele") @QueryParam("reference") String reference,
                                @ApiParam(value = "Main alternate allele") @QueryParam("alternate") String alternate,
//                                @ApiParam(value = "") @QueryParam("studies") String studies,
                                @ApiParam(value = "List of studies to be returned") @QueryParam("returnedStudies") String returnedStudies,
                                @ApiParam(value = "List of samples to be returned") @QueryParam("returnedSamples") String returnedSamples,
                                @ApiParam(value = "List of files to be returned.") @QueryParam("returnedFiles") String returnedFiles,
                                @ApiParam(value = "Variants in specific files") @QueryParam("files") String files,
                                @ApiParam(value = VariantQueryParam.FILTER_DESCR) @QueryParam("filter") String filter,
                                @ApiParam(value = "Minor Allele Frequency: [{study:}]{cohort}[<|>|<=|>=]{number}") @QueryParam("maf") String maf,
                                @ApiParam(value = "Minor Genotype Frequency: [{study:}]{cohort}[<|>|<=|>=]{number}") @QueryParam("mgf") String mgf,
                                @ApiParam(value = "Number of missing alleles: [{study:}]{cohort}[<|>|<=|>=]{number}") @QueryParam("missingAlleles") String missingAlleles,
                                @ApiParam(value = "Number of missing genotypes: [{study:}]{cohort}[<|>|<=|>=]{number}") @QueryParam("missingGenotypes") String missingGenotypes,
                                @ApiParam(value = "Specify if the variant annotation must exists.") @QueryParam("annotationExists") boolean annotationExists,
                                @ApiParam(value = "Samples with a specific genotype: {samp_1}:{gt_1}(,{gt_n})*(;{samp_n}:{gt_1}(,{gt_n})*)* e.g. HG0097:0/0;HG0098:0/1,1/1") @QueryParam("genotype") String genotype,
                                @ApiParam(value = VariantQueryParam.SAMPLE_DESCR) @QueryParam("samples") String samples,
                                @ApiParam(value = "Consequence type SO term list. e.g. missense_variant,stop_lost or SO:0001583,SO:0001578") @QueryParam("annot-ct") String annot_ct,
                                @ApiParam(value = "XRef") @QueryParam("annot-xref") String annot_xref,
                                @ApiParam(value = "Biotype") @QueryParam("annot-biotype") String annot_biotype,
                                @ApiParam(value = "Polyphen, protein substitution score. [<|>|<=|>=]{number} or [~=|=|]{description} e.g. <=0.9 , =benign") @QueryParam("polyphen") String polyphen,
                                @ApiParam(value = "Sift, protein substitution score. [<|>|<=|>=]{number} or [~=|=|]{description} e.g. >0.1 , ~=tolerant") @QueryParam("sift") String sift,
//                                @ApiParam(value = "") @QueryParam("protein_substitution") String protein_substitution,
                                @ApiParam(value = "Conservation score: {conservation_score}[<|>|<=|>=]{number} e.g. phastCons>0.5,phylop<0.1,gerp>0.1") @QueryParam("conservation") String conservation,
                                @ApiParam(value = "Population minor allele frequency: {study}:{population}[<|>|<=|>=]{number}") @QueryParam("annot-population-maf") String annotPopulationMaf,
                                @ApiParam(value = "Alternate Population Frequency: {study}:{population}[<|>|<=|>=]{number}") @QueryParam("alternate_frequency") String alternate_frequency,
                                @ApiParam(value = "Reference Population Frequency: {study}:{population}[<|>|<=|>=]{number}") @QueryParam("reference_frequency") String reference_frequency,
                                @ApiParam(value = "List of transcript annotation flags. e.g. CCDS, basic, cds_end_NF, mRNA_end_NF, cds_start_NF, mRNA_start_NF, seleno") @QueryParam("annot-transcription-flags") String transcriptionFlags,
                                @ApiParam(value = "List of gene trait association id. e.g. \"umls:C0007222\" , \"OMIM:269600\"") @QueryParam("annot-gene-trait-id") String geneTraitId,
                                @ApiParam(value = "List of gene trait association names. e.g. \"Cardiovascular Diseases\"") @QueryParam("annot-gene-trait-name") String geneTraitName,
                                @ApiParam(value = "List of HPO terms. e.g. \"HP:0000545\"") @QueryParam("annot-hpo") String hpo,
                                @ApiParam(value = "List of GO (Genome Ontology) terms. e.g. \"GO:0002020\"") @QueryParam("annot-go") String go,
                                @ApiParam(value = "List of tissues of interest. e.g. \"tongue\"") @QueryParam("annot-expression") String expression,
                                @ApiParam(value = "List of protein variant annotation keywords") @QueryParam("annot-protein-keywords") String proteinKeyword,
                                @ApiParam(value = "List of drug names") @QueryParam("annot-drug") String drug,
                                @ApiParam(value = "Functional score: {functional_score}[<|>|<=|>=]{number} e.g. cadd_scaled>5.2 , cadd_raw<=0.3") @QueryParam("annot-functional-score") String functional,

                                @ApiParam(value = "Returned genotype for unknown genotypes. Common values: [0/0, 0|0, ./.]") @QueryParam("unknownGenotype") String unknownGenotype,
//                                @ApiParam(value = "Limit the number of returned variants. Max value: " + VariantFetcher.LIMIT_MAX) @DefaultValue(""+VariantFetcher.LIMIT_DEFAULT) @QueryParam("limit") int limit,
//                                @ApiParam(value = "Skip some number of variants.") @QueryParam("skip") int skip,
                                @ApiParam(value = "Returns the samples metadata group by study. Sample names will appear in the same order as their corresponding genotypes.", required = false) @QueryParam("samplesMetadata") boolean samplesMetadata,
                                @ApiParam(value = "Count results", required = false) @QueryParam("count") boolean count,
                                @ApiParam(value = "Sort the results", required = false) @QueryParam("sort") boolean sort,
                                @ApiParam(value = "Group variants by: [ct, gene, ensemblGene]", required = false) @DefaultValue("") @QueryParam("groupBy") String groupBy,
                                @ApiParam(value = "Calculate histogram. Requires one region.", required = false) @DefaultValue("false") @QueryParam("histogram") boolean histogram,
                                @ApiParam(value = "Histogram interval size", required = false) @DefaultValue("2000") @QueryParam("interval") int interval,
                                @ApiParam(value = "Merge results", required = false) @DefaultValue("false") @QueryParam("merge") boolean merge) {


        List<QueryResult> queryResults = new LinkedList<>();
        try {
            List<String> idList = getIdList(fileIdCsv);
            AbstractManager.MyResources<File> resource = fileManager.getUids(idList, studyStr, sessionId);
//            String[] splitFileId = fileIdCsv.split(",");
            for (File file : resource.getResourceList()) {
                QueryResult queryResult;
                // Get all query options
                QueryOptions queryOptions = new QueryOptions(uriInfo.getQueryParameters(), true);
                Query query = VariantStorageManager.getVariantQuery(queryOptions);
                query.put(VariantQueryParam.FILE.key(), file.getUid());
                if (count) {
                    queryResult = variantManager.count(query, sessionId);
                } else if (histogram) {
                    queryResult = variantManager.getFrequency(query, interval, sessionId);
                } else if (StringUtils.isNotEmpty(groupBy)) {
                    queryResult = variantManager.groupBy(groupBy, query, queryOptions, sessionId);
                } else {
                    queryResult = variantManager.get(query, queryOptions, sessionId);
                }
                queryResults.add(queryResult);
            }
        } catch (Exception e) {
            return createErrorResponse(e);
        }
        return createOkResponse(queryResults);
    }

    private ObjectMap getResumeFileJSON(java.nio.file.Path folderPath) throws IOException {
        ObjectMap objectMap = new ObjectMap();

        if (Files.exists(folderPath)) {
            try (DirectoryStream<java.nio.file.Path> folderStream = Files.newDirectoryStream(folderPath, "*_partial")) {
                for (java.nio.file.Path partPath : folderStream) {
                    String[] nameSplit = partPath.getFileName().toString().split("_");
                    ObjectMap chunkInfo = new ObjectMap();
                    chunkInfo.put("size", Integer.parseInt(nameSplit[1]));
                    objectMap.put(nameSplit[0], chunkInfo);
                }
            }
        }
        return objectMap;
    }

    private List<java.nio.file.Path> getSortedChunkList(java.nio.file.Path folderPath) throws IOException {
        List<java.nio.file.Path> files = new ArrayList<>();
        try (DirectoryStream<java.nio.file.Path> stream = Files.newDirectoryStream(folderPath, "*_partial")) {
            for (java.nio.file.Path p : stream) {
                logger.info("adding to ArrayList: " + p.getFileName());
                files.add(p);
            }
        }
        logger.info("----ordered files length: " + files.size());
        Collections.sort(files, new Comparator<java.nio.file.Path>() {
            public int compare(java.nio.file.Path o1, java.nio.file.Path o2) {
                int id_o1 = Integer.parseInt(o1.getFileName().toString().split("_")[0]);
                int id_o2 = Integer.parseInt(o2.getFileName().toString().split("_")[0]);
                return id_o1 - id_o2;
            }
        });
        return files;
    }

    @POST
    @Path("/{file}/update")
    @ApiOperation(value = "Update some file attributes", position = 16, response = File.class,
            notes = "The entire file is returned after the modification. Using include/exclude query parameters is encouraged to avoid"
                    + " slowdowns when sending unnecessary information where possible")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "include", value = "Fields included in the response, whole JSON path must be provided",
                    example = "name,attributes", dataType = "string", paramType = "query"),
            @ApiImplicitParam(name = "exclude", value = "Fields excluded in the response, whole JSON path must be provided", example = "id,status", dataType = "string", paramType = "query")
    })
    public Response updatePOST(
            @ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam(value = "file") String fileIdStr,
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
            @QueryParam("study") String studyStr,
            @ApiParam(value = "Action to be performed if the array of samples is being updated.", defaultValue = "ADD")
            @QueryParam("samplesAction") ParamUtils.UpdateAction samplesAction,
            @ApiParam(value = "Action to be performed if the array of annotationSets is being updated.", defaultValue = "ADD")
            @QueryParam("annotationSetsAction") ParamUtils.UpdateAction annotationSetsAction,
            @ApiParam(name = "params", value = "Parameters to modify", required = true) FileUpdateParams updateParams) {
        try {
            ObjectMap params = updateParams.toFileObjectMap();

            if (samplesAction == null) {
                samplesAction = ParamUtils.UpdateAction.ADD;
            }
            if (annotationSetsAction == null) {
                annotationSetsAction = ParamUtils.UpdateAction.ADD;
            }

            Map<String, Object> actionMap = new HashMap<>();
            actionMap.put(FileDBAdaptor.QueryParams.SAMPLES.key(), samplesAction.name());
            actionMap.put(FileDBAdaptor.QueryParams.ANNOTATION_SETS.key(), annotationSetsAction);
            queryOptions.put(Constants.ACTIONS, actionMap);

            QueryResult<File> queryResult = fileManager.update(studyStr, fileIdStr, params, queryOptions, sessionId);
            queryResult.setId("Update file");
            return createOkResponse(queryResult);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @POST
    @Path("/{file}/annotationSets/{annotationSet}/annotations/update")
    @Consumes(MediaType.APPLICATION_JSON)
    @ApiOperation(value = "Update annotations from an annotationSet")
    public Response updateAnnotations(
            @ApiParam(value = "File id, name or path. Paths must be separated by : instead of /", required = true)
            @PathParam("file") String fileStr,
            @ApiParam(value = "Study [[user@]project:]study.") @QueryParam("study") String studyStr,
            @ApiParam(value = "AnnotationSet id to be updated.") @PathParam("annotationSet") String annotationSetId,
            @ApiParam(value = "Action to be performed: ADD to add new annotations; REPLACE to replace the value of an already existing "
                    + "annotation; SET to set the new list of annotations removing any possible old annotations; REMOVE to remove some "
                    + "annotations; RESET to set some annotations to the default value configured in the corresponding variables of the "
                    + "VariableSet if any.", defaultValue = "ADD") @QueryParam("action") ParamUtils.CompleteUpdateAction action,
            @ApiParam(value = "Json containing the map of annotations when the action is ADD, SET or REPLACE, a json with only the key "
                    + "'remove' containing the comma separated variables to be removed as a value when the action is REMOVE or a json "
                    + "with only the key 'reset' containing the comma separated variables that will be set to the default value"
                    + " when the action is RESET") Map<String, Object> updateParams) {
        try {
            if (action == null) {
                action = ParamUtils.CompleteUpdateAction.ADD;
            }

            return createOkResponse(catalogManager.getFileManager().updateAnnotations(studyStr, fileStr, annotationSetId,
                    updateParams, action, queryOptions, sessionId));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/link")
    @ApiOperation(value = "Link an external file into catalog.", hidden = true, position = 19, response = QueryResponse.class)
    public Response link(@ApiParam(value = "Uri of the file", required = true) @QueryParam("uri") String uriStr,
                         @ApiParam(value = "(DEPRECATED) Use study instead") @QueryParam("studyId") String studyIdStr,
                         @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias") @QueryParam("study") String studyStr,
                         @ApiParam(value = "Path where the external file will be allocated in catalog", required = true) @QueryParam("path") String path,
                         @ApiParam(value = "Description") @QueryParam("description") String description,
                         @ApiParam(value = "Create the parent directories if they do not exist") @DefaultValue("false") @QueryParam("parents") boolean parents,
                         @ApiParam(value = "Size of the folder/file") @QueryParam("size") long size,
                         @ApiParam(value = "Checksum of something") @QueryParam("checksum") String checksum) {
        try {

            if (StringUtils.isNotEmpty(studyIdStr)) {
                studyStr = studyIdStr;
            }

            logger.debug("study: {}", studyStr);

            path = path.replace(":", "/");

            ObjectMap objectMap = new ObjectMap()
                    .append("parents", parents)
                    .append("description", description);
            objectMap.putIfNotEmpty(FileDBAdaptor.QueryParams.CHECKSUM.key(), checksum);
            List<String> uriList = Arrays.asList(uriStr.split(","));

            List<QueryResult<File>> queryResultList = new ArrayList<>();
            logger.info("path: {}", path);
            if (uriList.size() == 1) {
                // If it is just one uri to be linked, it will return an error response if there is some kind of error.
                URI myUri = UriUtils.createUri(uriList.get(0));
                queryResultList.add(catalogManager.getFileManager().link(studyStr, myUri, path, objectMap, sessionId));
            } else {
                for (String uri : uriList) {
                    logger.info("uri: {}", uri);
                    try {
                        URI myUri = UriUtils.createUri(uri);
                        queryResultList.add(catalogManager.getFileManager().link(studyStr, myUri, path, objectMap, sessionId));
                    } catch (URISyntaxException | CatalogException | IOException e) {
                        queryResultList.add(new QueryResult<>("Link file", -1, 0, 0, "", e.getMessage(), Collections.emptyList()));
                    }
                }
            }
            return createOkResponse(queryResultList);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/unlink")
    @ApiOperation(value = "Unlink an external file from catalog.", hidden = true, position = 20, response = QueryResponse.class)
    public Response unlink(@ApiParam(value = "File id", required = true) @QueryParam("fileId") String fileIdStr,
                           @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                           @QueryParam("study") String studyStr) throws CatalogException {
        try {
            QueryResult<File> queryResult = catalogManager.getFileManager().unlink(studyStr, fileIdStr, sessionId);
            return createOkResponse(new QueryResult<>("unlink", 0, 1, 1, null, null, queryResult.getResult()));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @Deprecated
    @GET
    @Path("/{fileId}/relink")
    @ApiOperation(value = "Change file location. Provided file must be either STAGE or be an external file. [DEPRECATED]", hidden = true,
            position = 21)
    public Response relink(@ApiParam(value = "File Id") @PathParam("fileId") @DefaultValue("") String fileIdStr,
                           @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                           @QueryParam("study") String studyStr,
                           @ApiParam(value = "New URI", required = true) @QueryParam("uri") String uriStr,
                           @ApiParam(value = "Do calculate checksum for new files", required = false) @DefaultValue("false")
                           @QueryParam("calculateChecksum") boolean calculateChecksum) {
        try {
            URI uri = UriUtils.createUri(uriStr);
            CatalogIOManager ioManager = catalogManager.getCatalogIOManagerFactory().get(uri);

            if (!ioManager.exists(uri)) {
                throw new CatalogIOException("File " + uri + " does not exist");
            }

            AbstractManager.MyResource<File> resource = fileManager.getUid(fileIdStr, studyStr, sessionId);
            File file = resource.getResource();

            new FileUtils(catalogManager).link(file, calculateChecksum, uri, false, true, sessionId);
            file = catalogManager.getFileManager().get(file.getUid(), queryOptions, sessionId).first();
            file = FileMetadataReader.get(catalogManager).setMetadataInformation(file, null, new QueryOptions(queryOptions), sessionId,
                    false);

            return createOkResponse(new QueryResult<>("relink", 0, 1, 1, null, null, Collections.singletonList(file)));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{file}/refresh")
    @ApiOperation(value = "Refresh metadata from the selected file or folder. Return updated files.", position = 22,
            response = QueryResponse.class)
    public Response refresh(@ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam(value = "file") String fileIdStr,
                            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                            @QueryParam("study") String studyStr) {
        try {
            isSingleId(fileIdStr);
            AbstractManager.MyResource<File> resource = fileManager.getUid(fileIdStr, studyStr, sessionId);

            File file = resource.getResource();

            List<File> files;
            FileUtils catalogFileUtils = new FileUtils(catalogManager);
            FileMetadataReader fileMetadataReader = FileMetadataReader.get(catalogManager);
            if (file.getType() == File.Type.FILE) {
                File file1 = catalogFileUtils.checkFile(studyStr, file, false, sessionId);
                file1 = fileMetadataReader.setMetadataInformation(file1, null, new QueryOptions(queryOptions), sessionId, false);
                if (file == file1) {    //If the file is the same, it was not modified. Only return modified files.
                    files = Collections.emptyList();
                } else {
                    files = Collections.singletonList(file);
                }
            } else {
                List<File> result = catalogManager.getFileManager().getFilesFromFolder(fileIdStr, studyStr, null, sessionId).getResult();
                files = new ArrayList<>(result.size());
                for (File f : result) {
                    File file1 = fileMetadataReader.setMetadataInformation(f, null, new QueryOptions(queryOptions), sessionId, false);
                    if (f != file1) {    //Add only modified files.
                        files.add(file1);
                    }
                }
            }
            return createOkResponse(new QueryResult<>("refresh", 0, files.size(), files.size(), null, null, files));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @DELETE
    @Path("/delete")
    @ApiOperation(value = "Delete existing files and folders")
    @ApiImplicitParams({
//            @ApiImplicitParam(name = Constants.DELETE_EXTERNAL_FILES, value = "Delete files and folders from disk (only applicable for "
//                    + "linked files/folders)", dataType = "boolean", defaultValue = "false", paramType = "query"),
            @ApiImplicitParam(name = Constants.SKIP_TRASH, value = "Skip trash and delete the files/folders from disk directly (CANNOT BE"
                    + " RECOVERED)", dataType = "boolean", defaultValue = "false", paramType = "query")
    })
    public Response delete(
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
            @QueryParam("study") String studyStr,
            @ApiParam(value = "Comma separated list of file names") @QueryParam("name") String name,
            @ApiParam(value = "Comma separated list of paths") @QueryParam("path") String path,
            @ApiParam(value = "Available types (FILE, DIRECTORY)") @QueryParam("type") String type,
            @ApiParam(value = "Comma separated bioformat values. For existing bioformats see files/bioformats")
            @QueryParam("bioformat")String bioformat,
            @ApiParam(value = "Comma separated format values. For existing formats see files/formats")
            @QueryParam("format") String formats,
            @ApiParam(value = "Status") @QueryParam("status") String status,
            @ApiParam(value = "Directory under which we want to look for files or folders") @QueryParam("directory") String directory,
            @ApiParam(value = "Creation date (Format: yyyyMMddHHmmss)") @QueryParam("creationDate") String creationDate,
            @ApiParam(value = "Modification date (Format: yyyyMMddHHmmss)") @QueryParam("modificationDate") String modificationDate,
            @ApiParam(value = "Description") @QueryParam("description") String description,
            @ApiParam(value = "Size") @QueryParam("size") String size,
            @ApiParam(value = "Comma separated list of sample ids or names") @QueryParam("samples") String samples,
            @ApiParam(value = "Annotation, e.g: key1=value(;key2=value)") @QueryParam("annotation") String annotation,
            @ApiParam(value = "Job id that created the file(s) or folder(s)") @QueryParam("job.id") String jobId,
            @ApiParam(value = "Text attributes (Format: sex=male,age>20 ...)") @QueryParam("attributes") String attributes,
            @ApiParam(value = "Numerical attributes (Format: sex=male,age>20 ...)")  @QueryParam("nattributes") String nattributes,
            @ApiParam(value = "Release value") @QueryParam("release") String release) {
        try {
            query.remove("study");

            return createOkResponse(fileManager.delete(studyStr, query, queryOptions, sessionId));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

//    @GET
//    @Path("/{file}/delete")
//    @ApiOperation(value = "Delete file [NOT TESTED]", position = 23, response = QueryResponse.class)
//    public Response deleteGET(
//            @ApiParam(value = "File id, name or path. Paths must be separated by : instead of /") @PathParam(value = "file") String fileIdStr,
//            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
//                @QueryParam("study") String studyStr,
//            @ApiParam(value = "Delete files and folders from disk (only applicable for linked files/folders)")
//                @DefaultValue("false") @QueryParam("deleteExternal") boolean deleteExternal,
//            @ApiParam(value = "Skip trash and delete the files/folders from disk directly (CANNOT BE RECOVERED)")
//                @DefaultValue("false") @QueryParam("skipTrash") boolean skipTrash) {
//        try {
////            QueryOptions qOptions = new QueryOptions(queryOptions)
//            ObjectMap params = new ObjectMap()
//                    .append(FileManager.DELETE_EXTERNAL_FILES, deleteExternal)
//                    .append(FileManager.SKIP_TRASH, skipTrash);
//            List<QueryResult<File>> result = fileManager.delete(studyStr, fileIdStr, params, sessionId);
//            return createOkResponse(result);
//        } catch (Exception e) {
//            return createErrorResponse(e);
//        }
//    }

    @GET
    @Path("/groupBy")
    @ApiOperation(value = "Group files by several fields", position = 24, response = QueryResponse.class,
            notes = "Only group by categorical variables. Grouping by continuous variables might cause unexpected behaviour")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "count", value = "Count the number of elements matching the group", dataType = "boolean",
                    paramType = "query"),
            @ApiImplicitParam(name = "limit", value = "Maximum number of documents (groups) to be returned", dataType = "integer",
                    paramType = "query", defaultValue = "50")
    })
    public Response groupBy(@ApiParam(value = "Comma separated list of fields by which to group by.", required = true) @DefaultValue("")
                            @QueryParam("fields") String fields,
                            @ApiParam(value = "(DEPRECATED) Use study instead", hidden = true) @DefaultValue("") @QueryParam("studyId")
                                    String studyIdStr,
                            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                            @QueryParam("study") String studyStr,
                            @ApiParam(value = "Comma separated list of names.", required = false) @DefaultValue("") @QueryParam("name")
                                    String names,
                            @ApiParam(value = "Comma separated Type values.", required = false) @DefaultValue("") @QueryParam("type")
                                    String type,
                            @ApiParam(value = "Comma separated Bioformat values.", required = false) @DefaultValue("")
                            @QueryParam("bioformat") String bioformat,
                            @ApiParam(value = "Comma separated Format values.", required = false) @DefaultValue("") @QueryParam("format")
                                    String formats,
                            @ApiParam(value = "status", required = false) @DefaultValue("") @QueryParam("status") String status,
                            @ApiParam(value = "directory", required = false) @DefaultValue("") @QueryParam("directory") String directory,
                            @ApiParam(value = "creationDate", required = false) @DefaultValue("") @QueryParam("creationDate")
                                    String creationDate,
                            @ApiParam(value = "size", required = false) @DefaultValue("") @QueryParam("size") String size,
                            @ApiParam(value = "Comma separated sampleIds", hidden = true) @QueryParam("sampleIds") String sampleIds,
                            @ApiParam(value = "Comma separated list of sample ids or names") @QueryParam("samples") String samples) {
        try {
            query.remove("study");
            query.remove("fields");

            if (StringUtils.isNotEmpty(studyIdStr)) {
                studyStr = studyIdStr;
            }
            if (StringUtils.isNotEmpty(sampleIds) && !query.containsKey(FileDBAdaptor.QueryParams.SAMPLES.key())) {
                query.put(FileDBAdaptor.QueryParams.SAMPLES.key(), sampleIds);
            }
            QueryResult result = fileManager.groupBy(studyStr, query, fields, queryOptions, sessionId);
            return createOkResponse(result);
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{files}/acl")
    @ApiOperation(value = "Return the acl defined for the file or folder. If member is provided, it will only return the acl for the member.", position = 18, response = QueryResponse.class)
    public Response getAcls(@ApiParam(value = "Comma separated list of file ids or names up to a maximum of 100.", required = true)
                            @PathParam("files") String fileIdStr,
                            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias up to a maximum of 100")
                            @QueryParam("study") String studyStr,
                            @ApiParam(value = "User or group id") @QueryParam("member") String member,
                            @ApiParam(value = "Boolean to accept either only complete (false) or partial (true) results", defaultValue = "false") @QueryParam("silent") boolean silent) {
        try {
            List<String> idList = getIdList(fileIdStr);
            return createOkResponse(fileManager.getAcls(studyStr, idList, member, silent, sessionId));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    // Temporal method used by deprecated methods. This will be removed at some point.
    @Override
    protected File.FileAclParams getAclParams(
            @ApiParam(value = "Comma separated list of permissions to add", required = false) @QueryParam("add") String addPermissions,
            @ApiParam(value = "Comma separated list of permissions to remove", required = false) @QueryParam("remove") String removePermissions,
            @ApiParam(value = "Comma separated list of permissions to set", required = false) @QueryParam("set") String setPermissions)
            throws CatalogException {
        int count = 0;
        count += StringUtils.isNotEmpty(setPermissions) ? 1 : 0;
        count += StringUtils.isNotEmpty(addPermissions) ? 1 : 0;
        count += StringUtils.isNotEmpty(removePermissions) ? 1 : 0;
        if (count > 1) {
            throw new CatalogException("Only one of add, remove or set parameters are allowed.");
        } else if (count == 0) {
            throw new CatalogException("One of add, remove or set parameters is expected.");
        }

        String permissions = null;
        AclParams.Action action = null;
        if (StringUtils.isNotEmpty(addPermissions)) {
            permissions = addPermissions;
            action = AclParams.Action.ADD;
        }
        if (StringUtils.isNotEmpty(setPermissions)) {
            permissions = setPermissions;
            action = AclParams.Action.SET;
        }
        if (StringUtils.isNotEmpty(removePermissions)) {
            permissions = removePermissions;
            action = AclParams.Action.REMOVE;
        }
        return new File.FileAclParams(permissions, action, null);
    }

    @POST
    @Path("/{file}/acl/{memberId}/update")
    @ApiOperation(value = "Update the permissions granted for the user or group [DEPRECATED]", position = 21,
            hidden = true, response = QueryResponse.class,
            notes = "DEPRECATED: The usage of this webservice is discouraged. A different entrypoint /acl/{members}/update has been added "
                    + "to also support changing permissions using queries.")
    public Response updateAclPOST(
            @ApiParam(value = "File id, name or path. Paths must be separated by : instead of /", required = true) @PathParam("file") String fileIdStr,
            @ApiParam(value = "User or group id", required = true) @PathParam("memberId") String memberId,
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias") @QueryParam("study")
                    String studyStr,
            @ApiParam(value = "JSON containing one of the keys 'add', 'set' or 'remove'", required = true) StudyWSServer.MemberAclUpdateOld params) {
        try {
            File.FileAclParams aclParams = getAclParams(params.add, params.remove, params.set);
            List<String> idList = StringUtils.isEmpty(fileIdStr) ? Collections.emptyList() : getIdList(fileIdStr);
            return createOkResponse(fileManager.updateAcl(studyStr, idList, memberId, aclParams, sessionId));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    public static class FileAcl extends AclParams {
        public String file;
        public String sample;
    }

    @POST
    @Path("/acl/{members}/update")
    @ApiOperation(value = "Update the set of permissions granted for the member", position = 21)
    public Response updateAcl(
            @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias") @QueryParam("study")
                    String studyStr,
            @ApiParam(value = "Comma separated list of user or group ids", required = true) @PathParam("members") String memberId,
            @ApiParam(value = "JSON containing the parameters to add ACLs", required = true) FileAcl params) {
        try {
            ObjectUtils.defaultIfNull(params, new FileAcl());

            File.FileAclParams aclParams = new File.FileAclParams(
                    params.getPermissions(), params.getAction(), params.sample);
            List<String> idList = StringUtils.isEmpty(params.file) ? Collections.emptyList() : getIdList(params.file);
            return createOkResponse(fileManager.updateAcl(studyStr, idList, memberId, aclParams, sessionId));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    @GET
    @Path("/{folder}/scan")
    @ApiOperation(value = "Scans a folder", position = 6)
    public Response scan(@ApiParam(value = "Folder id, name or path. Paths must be separated by : instead of /") @PathParam("folder") String folderIdStr,
                         @ApiParam(value = "Study [[user@]project:]study where study and project can be either the id or alias")
                         @QueryParam("study") String studyStr,
                         @ApiParam(value = "calculateChecksum") @QueryParam("calculateChecksum") @DefaultValue("false")
                                 boolean calculateChecksum) {
        try {
            isSingleId(folderIdStr);
            AbstractManager.MyResource<File> resource = fileManager.getUid(folderIdStr, studyStr, sessionId);

            List<File> scan = new FileScanner(catalogManager)
                    .scan(resource.getResource(), null, FileScanner.FileScannerPolicy.REPLACE, calculateChecksum, false, sessionId);
            return createOkResponse(new QueryResult<>("Scan", 0, scan.size(), scan.size(), "", "", scan));
        } catch (Exception e) {
            return createErrorResponse(e);
        }
    }

    private static class FileCreateParams {
        @JsonProperty(required = true)
        public String path;
        public String content;
        public String description;
        @JsonProperty(defaultValue = "false")
        public boolean parents;
        @JsonProperty(defaultValue = "false")
        public boolean directory;
    }

    private static class FileUpdateParams {
        public String name;
        public String description;

        public List<String> samples;

        public String checksum;
        public File.Format format;
        public File.Bioformat bioformat;
        public Software software;

        public List<AnnotationSet> annotationSets;
        public Map<String, Object> stats;
        public Map<String, Object> attributes;

        public ObjectMap toFileObjectMap() throws JsonProcessingException {
            ObjectMapper mapper = new ObjectMapper();
            mapper.addMixIn(File.class, FileMixin.class);
            mapper.setSerializationInclusion(JsonInclude.Include.NON_NULL);

            File file = new File()
                    .setName(name)
                    .setDescription(description)
                    .setChecksum(checksum)
                    .setFormat(format)
                    .setBioformat(bioformat)
                    .setStats(stats)
                    .setSoftware(software)
                    .setAttributes(attributes);

            ObjectMap params = new ObjectMap(mapper.writeValueAsString(file));
            params.putIfNotNull("samples", samples);
            params.putIfNotNull(FileDBAdaptor.QueryParams.ANNOTATION_SETS.key(), annotationSets);

            return params;
        }
    }

}
