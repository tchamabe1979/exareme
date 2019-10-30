package madgik.exareme.master.engine.iterations.handler;

import madgik.exareme.master.client.AdpDBClientQueryStatus;
import madgik.exareme.master.connector.DataSerialization;
import madgik.exareme.master.engine.iterations.state.IterativeAlgorithmState;
import org.apache.http.entity.BasicHttpEntity;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.IOControl;
import org.apache.http.nio.entity.HttpAsyncContentProducer;
import org.apache.log4j.Logger;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.charset.StandardCharsets;

/**
 * @author Christos Aslanoglou <br> caslanoglou@di.uoa.gr <br> University of Athens / Department of
 * Informatics and Telecommunications.
 */
public class NIterativeAlgorithmResultEntity extends BasicHttpEntity
        implements HttpAsyncContentProducer {
    private static final Logger log = Logger.getLogger(NIterativeAlgorithmResultEntity.class);

    private IterativeAlgorithmState iterativeAlgorithmState;
    private AdpDBClientQueryStatus finalizeQueryStatus;
    // Used for signifying if IOCtrl of response has been registered with IterativeAlgorithmState.
    private boolean haveRegisteredIOCtrl;
    // Serialization format of the response.
    private DataSerialization dataSerialization;

    // Reading response related fields.
    private final ByteBuffer buffer;
    private ReadableByteChannel channel;

    public NIterativeAlgorithmResultEntity(IterativeAlgorithmState iterativeAlgorithmState,
                                           DataSerialization dataSerialization,
                                           int responseBufferSize) {
        this.iterativeAlgorithmState = iterativeAlgorithmState;
        haveRegisteredIOCtrl = false;
        // Buffer is transmitted to response stream, thus fixed size is needed
        buffer = ByteBuffer.allocate(responseBufferSize);
        channel = null;
        this.dataSerialization = dataSerialization;
    }

    private final static String user_error = new String("text/plain+user_error");
    private final static String error = new String("text/plain+error");
    private final static String warning = new String("text/plain+warning");


    /**
     * @param encoder is used to save the output
     * @param ioctrl  will be used from the iterativeAlgorithmState, when the algorithm is complete,
     *                to signal that the output is ready
     * @throws IOException if the output channel cannot be read
     */
    @Override
    public void produceContent(ContentEncoder encoder, IOControl ioctrl) throws IOException {

        if (!haveRegisteredIOCtrl) {
            // Registering IOCtrl to Iterative Algorithm State so as to be notified for
            // algorithm completion event.
            try {
                iterativeAlgorithmState.setIoctrl(ioctrl);
                ioctrl.suspendOutput();
                haveRegisteredIOCtrl = true;
            } finally {
                iterativeAlgorithmState.releaseLock();
            }
            return;
        }

        try {
            // This method will be called after calling ioctrl.requestOutput() from
            // AlgorithmCompletionEventHandler. Thus, the check below is simply for programming
            // errors.
            iterativeAlgorithmState.lock();

            if (iterativeAlgorithmState.getAlgorithmCompleted()) {
                // Iterative algorithm is complete, read response table and write it to the
                // communication channel, if no errors, otherwise write query errors.
                finalizeQueryStatus = iterativeAlgorithmState.getAdpDBClientQueryStatus();
                if (!finalizeQueryStatus.hasError() &&
                        finalizeQueryStatus.hasFinished()) {
                    if (channel == null) {
                        channel = Channels.newChannel(
                                iterativeAlgorithmState.getAdpDBClientQueryStatus()
                                        .getResult(dataSerialization));
                    }
                    // Reading from the channel to the buffer, flip is required by the API
                    channel.read(buffer);
                    buffer.flip();
                    int i = encoder.write(buffer);
                    final boolean buffering = this.buffer.hasRemaining();
                    this.buffer.compact();
                    if (i < 1 && !buffering) {
                        encoder.complete();
                    }
                } else {
                    encoder.write(ByteBuffer.wrap(
                            finalizeQueryStatus.getError().getBytes()));
                    encoder.complete();
                }
            } else {
                // Algorithm execution failed, notify the client.
                // this code was executed because ioctrl.requestOutput() was called from signifyAlgorithmError

                if (!iterativeAlgorithmState.getAlgorithmHasError()) {
                    // Should never happen.
                    String errMsg = "Attempt to produce response while " +
                            iterativeAlgorithmState.toString()
                            + " is still running.";
                    log.error(errMsg);
                    return;
                }

                // Catch whatever error coming from UDFs
                // The local algorithm steps will throw specific exceptions in order to show them to the user.
                // An ExaremeError is thrown if the algorithm developer wants to show an error message to the user.
                // A PrivacyError is caused due to insufficient data.
                String result = iterativeAlgorithmState.getAlgorithmError();
                if (result.contains("ExaremeError:")) {
                    String data = result.substring(result.lastIndexOf("ExaremeError:") + "ExaremeError:".length()).replaceAll("\\s", " ");
                    String type = user_error;
                    String output = defaultOutputFormat(data,type);
                    logErrorMessage(output);
                    channel = Channels.newChannel(
                            new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8)));

                } else if (result.contains("PrivacyError")) {
                    String data = "The Experiment could not run with the input provided because there are insufficient data.";
                    String type = warning;
                    String output = defaultOutputFormat(data,type);
                    logErrorMessage(output);
                    channel = Channels.newChannel(
                            new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8)));

                } else if (result.matches("java.rmi.RemoteException: Containers:.*not responding")) {
                    String data = "One or more containers are not responding. Please inform the system administrator.";
                    String type = error;
                    String output = defaultOutputFormat(data,type);
                    logErrorMessage(output);
                    channel = Channels.newChannel(
                            new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8)));

                } else {   // Unexpected error
                    String data = "Something went wrong with the execution of algorithm: ["
                            + iterativeAlgorithmState.getAlgorithmKey()
                            + "]. Please inform your system administrator to consult the logs.";
                    String type = error;
                    String output = defaultOutputFormat(data,type);
                    logErrorMessage(output);
                    channel = Channels.newChannel(
                            new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8)));
                }

                channel.read(buffer);
                buffer.flip();
                encoder.write(buffer);
                this.buffer.compact();
                encoder.complete();
            }
        } finally {
            if (iterativeAlgorithmState != null)
                iterativeAlgorithmState.releaseLock();
        }
    }

    @Override
    public void close() throws IOException {
        if (finalizeQueryStatus != null) {
            // Case in which algorithm execution failed
            finalizeQueryStatus.close();
            finalizeQueryStatus = null;
        }
        iterativeAlgorithmState = null;
    }

    @Override
    public boolean isRepeatable() {
        return false;
    }

    private String defaultOutputFormat(String data, String type){
        return "{\"result\" : [{\"data\":"+"\""+data+"\",\"type\":"+"\""+type+"\"}]}";
    }

    private void logErrorMessage(String error){
        log.info("Algorithm exited with error and returned:\n " + error);
    }
}
