package com.amazonaws.codepipeline.jobworker.configuration;

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadPoolExecutor;

import com.amazonaws.codepipeline.jobworker.CodePipelineJobPoller;
import com.amazonaws.codepipeline.jobworker.CodePipelineJobProcessor;
import com.amazonaws.codepipeline.jobworker.JobPoller;
import com.amazonaws.codepipeline.jobworker.JobProcessor;
import com.amazonaws.codepipeline.jobworker.JobService;
import com.amazonaws.services.codepipeline.AmazonCodePipelineClient;

/**
 * Default implementation for the job worker configuration. Can be used as a base configuration for different
 * job service implementations. Only the implementation of the job service has to be provided.
 * By default: sets the polling interval to 1000 ms and starts 10 worker threads.
 */
public abstract class DefaultJobWorkerConfiguration implements JobWorkerConfiguration {
    /**
     * The polling interval the daemon schedules the job poller which polls for new jobs.
     */
    private static final long POLL_INTERVAL_MS = 1000L;

    /**
     * Number of worker threads. Indicates how many jobs can be processed in parallel.
     */
    private static final int WORKER_THREADS = 10;

    /**
     * Number of jobs returned in a batch, should be typically the same like the worker threads.
     */
    private static final int POLL_BATCH_SIZE = WORKER_THREADS;

    /**
     * AWS CodePipeline preview VIP, only needed during private beta.
     */
    private static final String CODE_PIPELINE_ENDPOINT = "https://codepipeline-preview.us-east-1.amazonaws.com";

    /**
     * @return the poll interval in milliseconds
     */
    public long getPollingIntervalInMs() {
        return POLL_INTERVAL_MS;
    }

    /**
     * @return job poller implementation
     */
    public JobPoller jobPoller() {
        return new CodePipelineJobPoller(jobService(), jobProcessor(), threadPoolExecutor(), POLL_BATCH_SIZE);
    }

    /**
     * @return job processor implementation
     */
    protected JobProcessor jobProcessor() {
        return new CodePipelineJobProcessor();
    }

    /**
     * @return code pipeline client implementation
     */
    protected AmazonCodePipelineClient codePipelineClient() {
        AmazonCodePipelineClient codePipelineClient = new AmazonCodePipelineClient();
        codePipelineClient.setEndpoint(CODE_PIPELINE_ENDPOINT);
        return codePipelineClient;
    }

    /**
     * @return thread pool executor implementation
     */
    protected ThreadPoolExecutor threadPoolExecutor() {
        return (ThreadPoolExecutor) Executors.newFixedThreadPool(WORKER_THREADS);
    }

    /**
     * @return job service implementation
     */
    protected abstract JobService jobService();
}
