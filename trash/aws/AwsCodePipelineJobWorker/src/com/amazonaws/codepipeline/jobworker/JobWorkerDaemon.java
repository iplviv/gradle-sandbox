package com.amazonaws.codepipeline.jobworker;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.apache.commons.daemon.Daemon;
import org.apache.commons.daemon.DaemonContext;
import org.apache.commons.daemon.DaemonInitException;
import org.apache.log4j.Logger;

import com.amazonaws.codepipeline.jobworker.configuration.JobWorkerConfiguration;
import com.amazonaws.codepipeline.jobworker.configuration.ThirdPartyJobWorkerConfiguration;

/**
 * The daemon schedules the poller at a fixed time rate.
 */
public class JobWorkerDaemon implements Daemon {

    private static final Logger LOGGER = Logger.getLogger(JobWorkerDaemon.class);

    private final ScheduledExecutorService executorService;

    private JobPoller jobPoller;
    private long pollingIntervalInMs;

    /**
     * Initializes the daemon with default settings:
     * Scheduled Thread Pool with pool size 1 to invoke job poller on a fixed rate.
     * (Default every second)
     * Uses third party action configuration as a default.
     */
    public JobWorkerDaemon() {
        this(Executors.newScheduledThreadPool(1), new ThirdPartyJobWorkerConfiguration());
    }

    /**
     * Initializes daemon with a custom scheduled executor service and poller.
     * @param executorService scheduled executor service
     * @param jobWorkerConfiguration job worker configuration class defining settings and dependencies
     */
    public JobWorkerDaemon(final ScheduledExecutorService executorService, final JobWorkerConfiguration jobWorkerConfiguration) {
        Validator.notNull(executorService);
        Validator.notNull(jobWorkerConfiguration);
        this.executorService = executorService;
        initConfiguration(jobWorkerConfiguration);
    }

    /**
     * Initializes the daemon.
     * @param context daemon context.
     * @throws DaemonInitException exception during initialization
     */
    public void init(final DaemonContext context) throws DaemonInitException {
        LOGGER.info("Initialize daemon.");

        final String[] arguments = context.getArguments();
        if (arguments != null){
            LOGGER.debug(String.format("JobWorker arguments '%s'", String.join(", ", arguments)));
            loadConfiguration(arguments);
        }
    }

    /**
     * Starts the daemon. Initializes the executor service to execute the job poller at a fixed rate.
     * @throws Exception exception during start up
     */
    public void start() throws Exception {
        LOGGER.info("Starting up daemon.");

        executorService.scheduleAtFixedRate(jobPollerRunnable(),
                pollingIntervalInMs,
                pollingIntervalInMs,
                TimeUnit.MILLISECONDS);
    }

    /**
     * Stops the daemon. Shuts down the executor service gracefully.
     * Waits until the job poller and job processors finished their work.
     * @throws Exception exception during shutdown
     */
    public void stop() throws Exception {
        LOGGER.info("Stopping daemon.");

        this.executorService.shutdown();
        try {
            if (!this.executorService.awaitTermination(1, TimeUnit.MINUTES)) {
                this.executorService.shutdownNow();
                if (!this.executorService.awaitTermination(1, TimeUnit.MINUTES)) {
                    throw new IllegalStateException("Failed graceful shutdown of executor threads");
                }
            }
        } catch (InterruptedException e) {
            this.executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
        LOGGER.info("Stopped daemon.");
    }

    /**
     * Destroys the daemon.
     */
    public void destroy() {
        LOGGER.info("Destroying daemon.");
    }

    private Runnable jobPollerRunnable() {
        return () -> {
            try {
                jobPoller.execute();
            } catch (final RuntimeException e) { // NOPMD
                LOGGER.error("Caught exception while processing jobs", e);
            }
        };
    }

    private void loadConfiguration(final String[] arguments) throws DaemonInitException {
        if (arguments.length == 1) {
            final String configurationClassName = arguments[0];
            try {
                final JobWorkerConfiguration jobWorkerConfiguration = (JobWorkerConfiguration) Class.forName(configurationClassName).newInstance();
                initConfiguration(jobWorkerConfiguration);
            } catch (final InstantiationException | IllegalAccessException | ClassNotFoundException | ClassCastException e) {
                throw new DaemonInitException(
                        String.format("Provided job worker configuration class '%s' could not be loaded.", configurationClassName),
                        e);
            }
        }
    }

    private void initConfiguration(final JobWorkerConfiguration jobWorkerConfiguration) {
        this.jobPoller = jobWorkerConfiguration.jobPoller();
        this.pollingIntervalInMs = jobWorkerConfiguration.getPollingIntervalInMs();
    }
}
