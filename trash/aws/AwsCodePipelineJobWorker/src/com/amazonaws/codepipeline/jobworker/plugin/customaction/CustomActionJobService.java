package com.amazonaws.codepipeline.jobworker.plugin.customaction;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

import com.amazonaws.codepipeline.jobworker.JobService;
import com.amazonaws.codepipeline.jobworker.Validator;
import com.amazonaws.codepipeline.jobworker.model.ActionType;
import com.amazonaws.codepipeline.jobworker.model.CurrentRevision;
import com.amazonaws.codepipeline.jobworker.model.ExecutionDetails;
import com.amazonaws.codepipeline.jobworker.model.FailureDetails;
import com.amazonaws.codepipeline.jobworker.model.JobStatus;
import com.amazonaws.codepipeline.jobworker.model.WorkItem;
import com.amazonaws.codepipeline.jobworker.plugin.JobConverter;
import com.amazonaws.services.codepipeline.AmazonCodePipelineClient;
import com.amazonaws.services.codepipeline.model.AcknowledgeJobRequest;
import com.amazonaws.services.codepipeline.model.AcknowledgeJobResult;
import com.amazonaws.services.codepipeline.model.Job;
import com.amazonaws.services.codepipeline.model.PollForJobsRequest;
import com.amazonaws.services.codepipeline.model.PollForJobsResult;
import com.amazonaws.services.codepipeline.model.PutJobFailureResultRequest;
import com.amazonaws.services.codepipeline.model.PutJobSuccessResultRequest;

/**
 * Service interface wrapper for the custom action job api.
 */
public class CustomActionJobService implements JobService {
    private static final Logger LOGGER = Logger.getLogger(CustomActionJobService.class);

    private final AmazonCodePipelineClient codePipelineClient;
    private final ActionType actionType;

    /**
     * Initializes the custom action job service wrapper.
     * @param codePipelineClient service client for the AWS CodePipeline api.
     * @param actionType action type to poll for.
     */
    public CustomActionJobService(final AmazonCodePipelineClient codePipelineClient, final ActionType actionType) {
        Validator.notNull(codePipelineClient);
        Validator.notNull(actionType);

        this.codePipelineClient = codePipelineClient;
        this.actionType = actionType;
    }

    /**
     * Polls for jobs for the configured action type of the job worker.
     * @param maxBatchSize maximum number of jobs to be returned by the poll api.
     * @return List of work items.
     */
    public List<WorkItem> pollForJobs(final int maxBatchSize) {
        LOGGER.info(String.format("PollForJobs for action type %s", actionType));
        final List<WorkItem> result = new ArrayList<>();

        final PollForJobsRequest pollForJobsRequest = new PollForJobsRequest();
        pollForJobsRequest.setActionType(getActionType());
        pollForJobsRequest.setMaxBatchSize(maxBatchSize);

        final PollForJobsResult pollForJobsResult = codePipelineClient.pollForJobs(pollForJobsRequest);
        for (final Job job : pollForJobsResult.getJobs()) {
            result.add(JobConverter.convert(job));
        }
        return result;
    }

    /**
     * Acknowledges a job to indicate that the job worker started working on it.
     * If a job is not acknowledged in time it will be handed out another time by the poll for jobs api.
     * @param jobId job id
     * @param clientId aws account id
     * @param nonce job nonce
     * @return job status to indicate if the job worker should continue working on it
     */
    public JobStatus acknowledgeJob(final String jobId, final String clientId, final String nonce) {
        LOGGER.info(String.format("AcknowledgeJob for job '%s' and nonce '%s'", jobId, nonce));
        final AcknowledgeJobRequest request = new AcknowledgeJobRequest();
        request.setJobId(jobId);
        request.setNonce(nonce);
        final AcknowledgeJobResult result = codePipelineClient.acknowledgeJob(request);
        return JobStatus.valueOf(result.getStatus());
    }

    /**
     * Marks a job as successful.
     * @param jobId job id
     * @param clientId aws account id
     * @param executionDetails execution details
     * @param currentRevision current revision
     * @param continuationToken continuation token
     */
    public void putJobSuccess(final String jobId,
                              final String clientId,
                              final ExecutionDetails executionDetails,
                              final CurrentRevision currentRevision,
                              final String continuationToken) {
        LOGGER.info(String.format("PutJobSuccessResult for job '%s'", jobId));
        final PutJobSuccessResultRequest request = new PutJobSuccessResultRequest();
        request.setJobId(jobId);
        request.setExecutionDetails(JobConverter.convert(executionDetails));
        request.setCurrentRevision(JobConverter.convert(currentRevision));
        request.setContinuationToken(continuationToken);
        codePipelineClient.putJobSuccessResult(request);
    }

    /**
     * Marks a job as failed.
     * @param jobId job id
     * @param clientId aws account id
     * @param failureDetails failure details
     */
    public void putJobFailure(final String jobId, final String clientId, final FailureDetails failureDetails) {
        LOGGER.info(String.format("PutJobFailureResult for job '%s'", jobId));
        final PutJobFailureResultRequest request = new PutJobFailureResultRequest();
        request.setJobId(jobId);
        request.setFailureDetails(JobConverter.convert(failureDetails));
        codePipelineClient.putJobFailureResult(request);
    }

    private com.amazonaws.services.codepipeline.model.ActionType getActionType() {
        return JobConverter.convert(actionType);
    }
}
