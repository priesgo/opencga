/*
 * Copyright 2015 OpenCB
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

package org.opencb.opencga.client.rest;

import org.opencb.commons.datastore.core.ObjectMap;
import org.opencb.commons.datastore.core.QueryOptions;
import org.opencb.commons.datastore.core.QueryResponse;
import org.opencb.opencga.catalog.exceptions.CatalogException;
import org.opencb.opencga.catalog.models.Cohort;
import org.opencb.opencga.catalog.models.Sample;
import org.opencb.opencga.client.config.ClientConfiguration;

import java.io.IOException;

/**
 * Created by imedina on 24/05/16.
 */
public class CohortClient extends AbstractParentClient<Cohort> {

    private static final String COHORT_URL = "cohorts";

    protected CohortClient(String sessionId, ClientConfiguration configuration) {
        super(sessionId, configuration);

        this.category = COHORT_URL;
        this.clazz = Cohort.class;
    }

    public QueryResponse<Cohort> create(String studyId, String cohortName, ObjectMap params) throws CatalogException, IOException {
        addParamsToObjectMap(params, "studyId", studyId, "name", cohortName);
        return execute(COHORT_URL, "create", params, Cohort.class);
    }

    public QueryResponse<Sample> getSamples(String cohortId, QueryOptions options) throws CatalogException, IOException {
        return execute(COHORT_URL, cohortId, "samples", options, Sample.class);
    }

}