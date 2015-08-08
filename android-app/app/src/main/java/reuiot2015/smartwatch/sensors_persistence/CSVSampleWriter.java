package reuiot2015.smartwatch.sensors_persistence;

import android.os.Environment;
import android.util.Log;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.IllegalFormatException;

/** Stores samples collected from a Collector into a local file.
 *
 * @author Mario A. Gutierrez (mag262@txstate.edu)
 */
public class CSVSampleWriter implements SampleAccumulator.SampleAccumulationListener {
    private PrintWriter writer;

    public CSVSampleWriter(String[] header, String filename) {
        if (Environment.MEDIA_MOUNTED.equals(Environment.getExternalStorageState())) {
            // Get the public directory to store to.
            File publicDirectory = new File(Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOCUMENTS), SmartWatchValues.ALBUM_NAME + "/data");
            Log.d("CSVSampleWriter", "Save path is: " + publicDirectory.getAbsolutePath());

            if (publicDirectory.mkdirs()) Log.d("CSVSampleWriter", "Created file structure.");
            else Log.d("CSVSampleWriter", "Using existing file structure, or failed to create.");

            try {
                // Open a writer to write to the sample file.
                writer = new PrintWriter(new BufferedWriter(new FileWriter(new File(publicDirectory, filename))));

                // Artificially call this method to write the sample header to file.
                receiveAccumulatedSamples(new String[][] { header });
            } catch (IOException e) {  Log.e("CSVSampleWriter", e.getMessage()); }
        } else {
            Log.d("CSVSampleWriter", "External media is not mounted.");
        }
    }

    @Override
    public boolean receiveAccumulatedSamples(String[][] samples) {
            try {
                StringBuilder sb = new StringBuilder();
                synchronized (samples) {
                    for (String[] sample : samples) {
                        for (int j = 0; j < sample.length - 1; ++j)
                            sb.append(sample[j]).append(", ");
                        int last = sample.length - 1;
                        sb.append(sample[last]).append("\n");
                        writer.write(sb.toString()); // Write the formatted samples to file.
                        sb.setLength(0); // Clear contents of builder.
                    }
                }
                writer.flush();
                return true;
            } catch (IllegalFormatException | NullPointerException e) {
                Log.e("CSVSampleWriter", e.getMessage());
            }
        return false;
    }

    /** Closes the output stream to the file. */
    public void release() {
        if (writer != null) this.writer.close();
    }
}
