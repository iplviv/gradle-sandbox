package com.amazonaws.codepipeline.jobworker.plugin;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.List;
import java.util.Map;

import com.amazonaws.codepipeline.jobworker.model.AWSSessionCredentials;
import com.amazonaws.codepipeline.jobworker.model.JobData;
import com.amazonaws.services.codepipeline.model.ActionConfiguration;
import com.amazonaws.services.codepipeline.model.Artifact;

/**
 * Helper class to assert that job data structures contain the same data.
 * This class is used in the third party and custom action job service tests.
 */
public final class JobAssertion {

    private JobAssertion() {
        // Utility class
    }

    /**
     * Asserts that the given Job Worker Job Data structure contains the same data like the Job Data from the FrontEnd.
     * @param expectedJobData FrontEnd Job Data structure
     * @param actualJobData Job Worker Job Data structure
     */
    public static void assertJobDataEquals(final com.amazonaws.services.codepipeline.model.JobData expectedJobData, final JobData actualJobData) {
        assertEquals(expectedJobData.getContinuationToken(), actualJobData.getContinuationToken());
        assertActionConfiguration(expectedJobData.getActionConfiguration(), actualJobData.getActionConfiguration());
        assertArtifacts(expectedJobData.getInputArtifacts(), actualJobData.getInputArtifacts());
        assertArtifacts(expectedJobData.getOutputArtifacts(), actualJobData.getOutputArtifacts());
        assertActionCredentials(expectedJobData.getArtifactCredentials(), actualJobData.getArtifactCredentials());
    }

    /**
     * Asserts that the given Job Worker Job Data structure contains the same data like the Third Party Job Data from the FrontEnd.
     * @param expectedJobData FrontEnd Third Party Job Data structure
     * @param actualJobData Job Worker Job Data structure
     */
    public static void assertJobDataEquals(final com.amazonaws.services.codepipeline.model.ThirdPartyJobData expectedJobData, final JobData actualJobData) {
        assertEquals(expectedJobData.getContinuationToken(), actualJobData.getContinuationToken());
        assertActionConfiguration(expectedJobData.getActionConfiguration(), actualJobData.getActionConfiguration());
        assertArtifacts(expectedJobData.getInputArtifacts(), actualJobData.getInputArtifacts());
        assertArtifacts(expectedJobData.getOutputArtifacts(), actualJobData.getOutputArtifacts());
        assertActionCredentials(expectedJobData.getArtifactCredentials(), actualJobData.getArtifactCredentials());
    }

    private static void assertActionCredentials(final com.amazonaws.services.codepipeline.model.AWSSessionCredentials expectedActionCredentials, final AWSSessionCredentials actualActionCredentials) {
        if (expectedActionCredentials == null) {
            assertNull(actualActionCredentials);
        } else {
            assertEquals(expectedActionCredentials.getAccessKeyId(), actualActionCredentials.getAccessKeyId());
            assertEquals(expectedActionCredentials.getSecretAccessKey(), actualActionCredentials.getSecretAccessKey());
            assertEquals(expectedActionCredentials.getSessionToken(), actualActionCredentials.getSessionToken());
        }
    }

    private static void assertActionConfiguration(final ActionConfiguration expectedActionConfiguration, final Map<String, String> actualActionConfiguration) {
        if (expectedActionConfiguration == null || expectedActionConfiguration.getConfiguration() == null) {
            assertEquals(0, actualActionConfiguration.size());
        } else {
            assertThat(expectedActionConfiguration.getConfiguration().entrySet(), equalTo(actualActionConfiguration.entrySet()));
        }
    }

    private static void assertArtifacts(final List<Artifact> expectedArtifacts, final List<com.amazonaws.codepipeline.jobworker.model.Artifact> actualArtifacts) {
        assertThat(expectedArtifacts, equalTo(actualArtifacts));
    }
}
