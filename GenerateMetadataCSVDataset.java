import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.file.Files;

public class GenerateMetadataCSVDataset {

	static IOConstants io = new IOConstants();

	public static void main(String[] args) {

		File[] dirs = io.getListOfSamplesDirs();
		new File(IOConstants.IMAGE_INPUT_DIR_ROOT + "\\Metadata_CSV\\").mkdir();
		File metadata_dir = new File(IOConstants.IMAGE_INPUT_DIR_ROOT
				+ "\\Metadata_CSV\\");
		new File(IOConstants.IMAGE_INPUT_DIR_ROOT + "\\Metadata_CSV\\FILTERED_IMAGE_FEATURES\\").mkdir();
		File metadata_dir2 = new File(IOConstants.IMAGE_INPUT_DIR_ROOT
				+ "\\Metadata_CSV\\FILTERED_IMAGE_FEATURES\\");

		for (File d : dirs) {
			File[] files = d.listFiles(new FilenameFilter() {
				public boolean accept(File arg0, String arg1) {
					return arg1.toLowerCase().endsWith("_meta.csv");
				}
			});
			for (File f : files) {
				System.out.println("Found and copying " + f.getName() + " to "+ new File(metadata_dir.getAbsolutePath() + "\\"+ f.getName()) );
				try {
					copyFile(f, new File(metadata_dir.getAbsolutePath() + "\\" + f.getName()));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

		for (File dd : dirs) {
			File[] files2 = null;
			files2 = dd.listFiles(new FilenameFilter() {
				public boolean accept(File arg0, String arg1) {
					boolean flag = arg1.toLowerCase().startsWith("results") || arg1.toLowerCase().startsWith("summary") || arg1.toLowerCase().startsWith("blobs")|| arg1.toLowerCase().startsWith("statistics");
					return (flag);
				}
			});
			for (File ff : files2) {
				System.out.println("Found and copying " + ff.getName() + " to "+ new File(metadata_dir2.getAbsolutePath() + "\\"+ ff.getName()) );
				try {
					copyFile(ff, new File(metadata_dir2.getAbsolutePath() + "\\" + ff.getName()));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

		
	}
		

	public static void copyFile( File from, File to ) throws IOException {
	    Files.copy( from.toPath(), to.toPath() );
	}
}
