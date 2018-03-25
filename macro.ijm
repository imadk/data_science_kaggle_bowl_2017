root_dir = "D:\\Kaggle\\Bowl2017\\image_input\\stage1_images\\";
macro_code = 4;

function printLog(log_file, string_log) {
    File.append(string_log, log_file);
}

function zoomout() {
    setLocation(0, 0);
    i = 1;
    do {
        run("Out");
        i++;
    } while (i < 3);
}

function dilate_slice() {
	run("Dilate", "slice");
}
function invert_slice(maxval) {
	run("Macro...", "code=v="+maxval+"-v slice");
}

function arrangeAndSessionize2(winname, sess) {
    if (isOpen(winname)) {
        selectWindow(winname);
        newname = winname + "_" + sess;
        rename(newname);
        if (isOpen(newname)) {
            selectWindow(newname);
        }
        return newname;
    }
    return "";
}

function arrangeAndSessionize(winname, sess) {
    if (isOpen(winname)) {
        selectWindow(winname);
        newname = winname; // + "_" + sess; //unfortunately bugs in getResult so no rename 
        if (winname == "Results") {
            IJ.renameResults(newname);         } else {
            rename(newname);
        }
        setLocation(0, 0);
        if (isOpen(newname)) {
            selectWindow(newname);
        }
        return newname;
    }
    return "";
}

function closeWin(winname) {
    if (isOpen(winname)) {
        selectWindow(winname);
        run("Close");
    }
}

function histogram_for_result_variable(results_name_title, variable, nBins) {
    selectWindow(results_name_title);
    n = nResults;
    if (n == 0) {
        return -1;
    }
    setBatchMode("hide");
    if (isNaN(getResult(variable, 0))) {
        return -1;
    }
    newImage(variable, "16-bit", n, 1, 1);
    for (i = 0; i < n; i++)
        setPixel(i, 0, getResult(variable, i));
    run("Histogram", "bins=" + nBins + " use y_max=Auto");
    id = getImageID;
    selectImage(variable);
    close();
    selectImage(id);
    setBatchMode("display and exit");
}

list = getFileList(root_dir);

start = getTime();

//close leftover windows that could not be closed //--Begin--//
if (isOpen("dup")) {
    selectWindow("dup");
    run("Close");
}
/* summary of --1 to be closes todo
if (isOpen(getTitle())) {
	selectWindow(getTitle());
	run("Close");
}
*/
//--End--//

session = 1;

LFile = File.open("D:\\Kaggle\\Bowl2017\\Log_" + session + ".txt");
LogFile = "D:\\Kaggle\\Bowl2017\\Log_" + session + ".txt";
setBatchMode("hide");
//for (iiii = list.length - floor(list.length / 3); iiii < list.length; iiii++) {
  for (iiii = 0; iiii < 1; iiii++) {
    /*if (iiii % 100 == 0) {
        call("java.lang.System.gc");
    }*/
    //    wait(5);
    dcm_dir = list[iiii];
    dcm_dir = replace(dcm_dir, "/", "");
    list_files = getFileList(root_dir + dcm_dir);
    dcm_file = list_files[0];

    //create dir for saving output images
    image_output_dir = dcm_dir + "/IMAGE_OUTPUTS/";
    image_output_tiles_dir = dcm_dir + "/IMAGE_OUTPUTS/TILES/";
    list2 = getFileList(root_dir + image_output_dir);
    list3 = getFileList(root_dir + image_output_tiles_dir);

//    File.makeDirectory(root_dir + image_output_dir);
  //  File.makeDirectory(root_dir + image_output_tiles_dir);


    if (dcm_dir != "NORMAL_00cba091fa4ad62cc3200a657aeb957e") {
        //	if(dcm_dir =="CANCER_0c0de3749d4fe175b7a5098b060982a1"){
        if (true) {
            printLog(LogFile, "");
            printLog(LogFile, "Processing now: " + dcm_dir + " " + (iiii + 1) + " / " + list.length + " of " + list.length);
            printLog(LogFile, "************************************************************************************************");

			wait(10);

            SCANNER_HIGH_BIT_QUALITY = 0;

            clearedall = 0;
            clearedoutside = 0;

            IJ.redirectErrorMessages();

            start2 = getTime();

            run("Image Sequence...", "open=" + root_dir + dcm_dir + "\\" + dcm_file + " sort");
            //hack -- for proper dicom window center and window width parameters reading
            printLog(LogFile, "-Fix for proper dicom window center and window width parameters reading...");
            setBatchMode("hide");
            run("Measure");
            name = "Results"; //arrangeAndSessionize("Results", session);
            minimum = parseFloat(getResult("Min"));
            maximum = parseFloat(getResult("Max"));
            setMinAndMax(minimum + 25, maximum + 50);
            setMinAndMax(minimum, maximum);

wait(5);
			//reslize z to 1.5mm (-- high quality is usually 1-2 mm thickness --)
			the_title = getTitle();
			run("Reslice Z", "new=1.5");
			rename(the_title);
wait(5);
			selectWindow(the_title);
			close();

            getDimensions(width, height, channels, slices, frames);
            detected_top_lobe_slice = 0;
            detected_bottom_lobe_slice = slices;

            setBatchMode("exit and display");
            //Stack stats
            selectWindow(dcm_dir);
            run("Original Scale");
            zoomout();

            run("Statistics");
            name2 = arrangeAndSessionize("Statistics of " + dcm_dir, session);
            saveAs("Results", root_dir + dcm_dir + "\\Statistics of " + dcm_dir + ".csv");
            minstack = parseFloat(getResult("Min"));
            closeWin(name);
            closeWin(name2);
            if (minstack == -32768) {
                SCANNER_HIGH_BIT_QUALITY = 1;
            }

            getDimensions(width, height, channels, slices, frames);
            //color calibration / normalization:
            //use middle of stack and maxima pixel intensities to know the background color and tissues-color ////////(most data-sample-agnostic)
            //4 frames too ; should be enough
            printLog(LogFile, "-Color calibration / normalization pre-processing...");
            maxi = newArray(-999999, -999999, -999999, -999999);
            mini = newArray(999999, 999999, 999999, 999999);
            ii = -1;
            setBatchMode("hide");
            for (s = slices / 2; s <= slices / 2 + 3; s++) {
                ii++;
                Stack.setSlice(s);
                run("Measure");
            name = "Results"; //arrangeAndSessionize("Results", session);
                maxi[ii] = parseFloat(getResult("Max"));
                mini[ii] = parseFloat(getResult("Min"));
                closeWin(name);
            }
            setBatchMode("exit and display");

            setBatchMode("hide");
            printLog(LogFile, "-maximum background intensities: " + maxi[0] + "|" + maxi[1] + "|" + maxi[2] + "|" + maxi[3]);
            maxi_avg = (0 + maxi[0] + maxi[1] + maxi[2] + maxi[3]) / 4;
            printLog(LogFile, "-minimum background intensities: " + mini[0] + "|" + mini[1] + "|" + mini[2] + "|" + mini[3]);
            mini_avg = (0 + mini[0] + mini[1] + mini[2] + mini[3]) / 4;

            BLACK = mini_avg;
            MAX = maxi_avg;
            MIN = mini_avg;
            NEW_MIN = MIN;
            if (SCANNER_HIGH_BIT_QUALITY == 1) {
                BLACK = mini_avg + 32768;
                MAX = maxi_avg + 32768;
                MIN = mini_avg + 32768;
                NEW_MIN = MIN;
            }


            printLog(LogFile, "-avg. maximum background intensity: " + MAX);
            printLog(LogFile, "-avg. minimum background intensity: " + MIN);

            setBatchMode("exit and display");
            setBatchMode("hide");

            //printLog(LogFile,"-HU Subtractor: " + HU_SUBTRACTOR);
            printLog(LogFile, "-MAX: " + MAX);
            printLog(LogFile, "-MIN: " + NEW_MIN);
            HU_SUBTRACTOR = -1024; //wrong--hardcoded dummy for now
            Stack.setFrame(1);
            Stack.setSlice(1);
            Stack.setChannel(1);
         /*   orientation_meta = getInfo("0020,0037");
            orientation_row = split(orientation_meta, "\\");
            orientation_row = orientation_row[0];
            orientation_row = parseInt(orientation_row);
            orientation_col = split(orientation_meta, "\\");
            orientation_col = orientation_col[1];
            orientation_col = split(orientation_col, "\\");
            orientation_col = orientation_col[0];
            orientation_col = parseInt(orientation_col);
            printLog(LogFile, "-Orientation row: " + orientation_row); 
            printLog(LogFile, "-Orientation col: " + orientation_col); 
            */
            setBatchMode("exit and display");

            //managing shiny artifacts...
            printLog(LogFile, "-Removing shiny artifacts if they exist...");
            setBatchMode("hide");

            if (SCANNER_HIGH_BIT_QUALITY == 0) {
                run("Macro...", "code=[if(v>2000 + 1024) {v=-1024;}] stack"); //hardcoded -1024 //change-optionally-todo? 
            } else {
                run("Macro...", "code=[if(v>2000 + 32768) {v=-1024;}] stack"); //hardcoded -1024 //change-optionally-todo?//0?           	
            }
            setBatchMode("exit and display");

            if (MIN < -1024) {
                //	        if (circle > 0) {
                printLog(LogFile, "  -circular ct has been detected with high probability -- or artifact\\shiny artifact present (less probable).");
                printLog(LogFile, "  -background has been adjusted.");
                //     	 }
            }

            HU_SUBTRACTOR = -1024;
            //color normalization \ calibration
            //TODO: printLog(LogFile,"-Color normalization \\ calibration...");
            //TODO: run( "Subtract...", "value=" + (BLACK + mini_avg)/2 - maxi_avg); //TODO

            setBatchMode("hide");
            //slices color normalization (for later processing & convnets)
            //+ stack CLAHE 
            //+ stack sharpen
            //+ detect first-slice v.s. last-slice pattern 
            //+ acquire mean value of each slice (for later processing -- convnet & other)
            printLog(LogFile, "-Slices color normalization (for later processing & convnets...");
            printLog(LogFile, "-Local Contrast Enhancement (Default values) & First-slices/last-slices instances detection...");
            setBatchMode("hide");
            getDimensions(width, height, channels, slices, frames);
            first_slice = -1;
            last_slice = -1;
            blocksize = 127;
            histogram_bins = 256;
            maximum_slope = 3;
            mask = "*None*";
            fast = true;
            process_as_composite = true;
            isComposite = channels > 1;
            parameters =
                "blocksize=" + blocksize +
                " histogram=" + histogram_bins +
                " maximum=" + maximum_slope +
                " mask=" + mask;
            if (fast)
                parameters += " fast_(less_accurate)";
            if (isComposite && process_as_composite) {
                parameters += " process_as_composite";
                channels = 1;
            }

            mean_val_f = 0;
            mean_val_l = 0;
            for (s = 1; s <= slices; s++) {
                wait(5);
                //                run("Measure");
                //                mean_val = parseFloat(getResult("Mean"));
                //                mean_avg += mean_val;
                //                if (isOpen("Results")) {
                //                    selectWindow("Results");
                //                    run("Close");
                //                }
                Stack.setSlice(s);
                if ((s <= 2)) {
                    run("Measure");
            name = "Results"; //arrangeAndSessionize("Results", session);
                    mean_val_f = parseFloat(getResult("Mean"));
                    closeWin(name);
                }
                //              wait(5); //wait(60);
                if ((s >= slices - 1)) {
                    run("Measure");
            name = "Results"; //arrangeAndSessionize("Results", session);
                    mean_val_l = parseFloat(getResult("Mean"));
                    closeWin(name);
                }

                //run("Enhance Local Contrast (CLAHE)", parameters); //TODO-OPTIONAL
                //run( "Subtract...", "value=" + mean_val - HU_SUBTRACTOR); //TODO? 			            
                //run("Subtract...", "value=" + mean_val); //TODO-OPTIONALs
            }
            setBatchMode("exit and display");

            setBatchMode("hide");
            run("Statistics");
            name = "Results"; //arrangeAndSessionize("Results", session);
            mean_avg = getResult("Mean");
            closeWin(name);
            if (SCANNER_HIGH_BIT_QUALITY == 1) {
                mean_avg += 32768;
            }
            printLog(LogFile, "-original avg. mean background intensity: " + (mean_avg));
            run("Enhance Contrast", "saturated=0.35 stack"); ////

            //run("Sharpen", "stack"); //optional step?

            //adjust mininim value (some circular cts are -3024 outside and others for e.g. -1000 inside...)
            if (MIN != -1024) {
                run("Min...", "value=-1024 stack");
                printLog(LogFile, "-Adjusting minimum value (to -1024) ...");
                NEW_MIN = -1024;
            }
            printLog(LogFile, "-MAX: " + MAX);
            printLog(LogFile, "-MIN: " + NEW_MIN);

            run("Statistics");
            name = "Results"; //arrangeAndSessionize("Results", session);
            mean_avg = getResult("Mean");
            closeWin(name);

            if (SCANNER_HIGH_BIT_QUALITY == 1) {
                mean_avg += 32768;
            }
            printLog(LogFile, "-avg. mean background intensity: " + (mean_avg));

            //            run("Subtract...", "value=" + mean_avg); //optional step?

            //detect first-slice v.s. last-slice pattern
            if (mean_val_f > mean_val_l) {
                first_slice = slices;
                printLog(LogFile, "-first slice is " + slices);
                last_slice = 1;
                printLog(LogFile, "-last slice is " + 1);
            } else {
                first_slice = 1;
                printLog(LogFile, "-first slice is " + 1);
                last_slice = slices;
                printLog(LogFile, "-last slice is " + slices);
            }

            //Reverse stack if it is in reverse order:
            reversed = 0;
            printLog(LogFile, "-Reverse stack if it is in reverse order");
            if (first_slice > 1) {
                temp_val = mean_val_f;
                mean_val_f = mean_val_l;
                mean_val_l = temp_val;
                run("Reverse");
                reversed = 1;
            }


            setBatchMode("exit and display");

            selectWindow(dcm_dir);
            //  zoomout();

            setBatchMode("hide");
            //Rotate for horizontal adjustment 
            printLog(LogFile, "-Rotate for horizontal adjustment based on detected orientation...");
            run("Duplicate...", "duplicate");
            selectWindow(dcm_dir + "-1");
            zoomout();
            setBatchMode("exit and display");


            setBatchMode("hide");

            Stack.setFrame(last_slice);
            setOption("BlackBackground", false);
            run("Make Binary", "method=Default background=Default calculate");

            run("Find Edges", "slice");
            y1 = -1;
            y2 = -1;
            slope = -1;
            for (y = 0; y < getHeight(); y++) {
                if (getPixel(getWidth() / 3, y) == 255) {
                    y1 = y;
                    break;
                }
            }
            for (z = 0; z < getHeight(); z++) {
                if (getPixel(2 * getWidth() / 3, z) == 255) {
                    y2 = z;
                    break;
                }
            }

            if (y1 != -1 && y2 != -1) {
                slope = y2 - y1;
            }
            setBatchMode("exit and display");

            selectWindow(dcm_dir + "-1");
            run("Original Scale");
            zoomout();

            setBatchMode("hide");
            Stack.setFrame(first_slice);
            setOption("BlackBackground", false);
            run("Make Binary", "method=Default background=Default calculate");
            run("Find Edges", "slice");
            y12 = -1;
            y22 = -1;
            slope2 = -1;
            for (y = 0; y < getHeight(); y++) {
                if (getPixel(getWidth() / 3, y) == 255) {
                    y12 = y;
                    break;
                }
            }
            for (z = 0; z < getHeight(); z++) {
                if (getPixel(2 * getWidth() / 3, z) == 255) {
                    y22 = z;
                    break;
                }
            }

            if (y1 != -1 && y2 != -1) {
                slope2 = y22 - y12;
            }


            if (isOpen(dcm_dir + "-1")) {
                selectWindow(dcm_dir + "-1");
                run("Close");
            }

            printLog(LogFile, "Detected vertical slope last slice: " + slope);
            printLog(LogFile, "Detected vertical slope first slice: " + slope2);

            setBatchMode("exit and display");
            selectWindow(dcm_dir);
            run("Original Scale");
            zoomout();

            setBatchMode("hide");
            if (slope != -1 && slope2 != -1) {
                the_angle = 57.2957795131 * atan((slope + slope2) / (2 * getWidth() / 3));
                //some patients are too obese they get out of the window and the algorithm fails -- in this case don't rotate
                if (the_angle < -7) {
                    the_angle = 0;
                }
                if (the_angle > 7) {
                    the_angle = 0;
                }
                printLog(LogFile, "Rotating by average slope (deg.): " + the_angle);
                run("Rotate... ", "angle=" + the_angle + " grid=1 interpolation=Bicubic stack");
            }
            setBatchMode("exit and display");

            selectWindow(dcm_dir);
            run("Original Scale");
            zoomout();
            setLocation(0, 0);


            //          run("Duplicate...", "duplicate range="+detected_top_lobe_slice+"-"+detected_bottom_lobe_slice);			
            //			selectWindow(getTitle());


            run("Duplicate...", "duplicate");
            run("Duplicate...", "duplicate");
            run("Duplicate...", "duplicate");
            //            run("Duplicate...", "duplicate");

            selectWindow(dcm_dir + "-2");
            run("Original Scale");
            zoomout();
            zoomout();

            selectWindow(dcm_dir + "-3");
            run("Original Scale");
            zoomout();
            zoomout();

            selectWindow(dcm_dir + "-1");
            run("Original Scale");
            zoomout();
            setLocation(0, 0);

            setBatchMode("hide");
            //Make binary using Moments
            printLog(LogFile, "-Half-stack slice analysis...");
            setOption("BlackBackground", false);
            run("Make Binary", "method=Default background=Default calculate");
            run("Auto Threshold", "method=Default black stack use_stack_histogram");


            //run("Erode (3D)", "iso=255");
            //run("Dilate (3D)", "iso=255");
            //MIN_NODULE_HU = -40;
            //                            run("Macro...", "code=[if(v<MIN_NODULE_HU) {v=-1024;}] stack");

            //Dilate in 3D and threshold
            /*run("Maximum 3D...", "x=2 y=2 z=2");
                            setOption("BlackBackground", true);
                            percentage = 0.98;
                            target = percentage * getWidth() * getHeight();
                            nBins = MAX;
                            getHistogram(values, counts, nBins);
                            sum = 0;
                            threshold = -1;
                            for (i = 0; i < nBins - 1; i++) {
                                sum += counts[i];
                                if ((sum >= target) & (threshold < 0)) {
                                    threshold = i;
                                }
                            }
                            setThreshold(NEW_MIN, threshold);

                            run("Convert to Mask", "method=Default background=Default calculate");
*/ //TODO
            //     wait(5);
            run("Find Edges", "stack");
            run("Original Scale");
            zoomout();
            //            printLog(LogFile, "*Saving result-image to disk...");
            //		   		 run("Image Sequence... ", "format=TIFF save="+sub_sub_tile_dir + title + "_" + "_s-" + s + "_x-" + x + "_y-" + y + ".tif");
            setBatchMode("exit and display");

            run("Duplicate...", "duplicate");
            //        drawings_title = getTitle() + "_DRAWINGS";
            //      rename(drawings_title);
            //    selectWindow(drawings_title);
            //    run("Original Scale");
            //    zoomout();
            //    zoomout();

            closeWin(dcm_dir + "-4");
            selectWindow(dcm_dir + "-1");
            run("Original Scale");
            zoomout();


            //            run("Duplicate...", "duplicate range="+detected_top_lobe_slice+"-"+detected_bottom_lobe_slice);			
            //			selectWindow(getTitle());

            getDimensions(width, height, channels, slices, frames);
            Stack.setSlice(slices / 2);
            run("Duplicate...", "title=dup_" + session);
            if (isOpen("dup_" + session)) {
                selectWindow("dup_" + session);
                zoomout();
                printLog(LogFile, "*Saving result-image to disk...");
                //     run("Bio-Formats Exporter", "save=" + root_dir + image_output_dir + "HALF-STACK_SLICE" + ".tif" + " write_each_z_section compression=Uncompressed");
                run("Close");
            }
            selectWindow(dcm_dir + "-1");
            run("Original Scale");
            zoomout();

            setBatchMode("hide");
            run("Analyze Particles...", "  show=Outlines display include exclude");
            name = arrangeAndSessionize("Half-Stack Lobes of " + dcm_dir + ".csv", session);
            name2 = "Results"; //arrangeAndSessionize("Results", session);

            saveAs("Results", root_dir + dcm_dir + "\\Half-Stack Lobes of " + dcm_dir + ".csv");
            closeWin(name);
            closeWin("Half-Stack Lobes of " + dcm_dir + ".csv");
            closeWin("Half-Stack Lobes of " + dcm_dir + ".csv" + "_" + session);            


		Lobes_Aprx_Area = -1;
	    if (nResults>1){ 
	        Area=newArray(nResults); 
   		     max_area = -1;
    	    for (i=0; i<nResults;i++){ 
        	      Area[i]=  getResult("Area",i); 
           	   if(Area[i]>max_area){
           	   	max_area =Area[i];
           	   }
       		 }       
       }
       Lobes_Aprx_Area = max_area;
		print(iiii, " - ", Lobes_Aprx_Area);        
            closeWin(name2);



            if (isOpen("Drawing of " + dcm_dir + "-1")) {
                selectWindow("Drawing of " + dcm_dir + "-1");
                run("Close");
            }

            setBatchMode("exit and display");



            selectWindow(dcm_dir + "-2");
            run("Original Scale");
            zoomout();
            setLocation(150, 500);

            selectWindow(dcm_dir + "-3");
            run("Original Scale");
            zoomout();
            setLocation(450, 500);

            selectWindow(dcm_dir);
            run("Original Scale");
            zoomout();
            setLocation(450, 150);


            setBatchMode("hide");
            if (SCANNER_HIGH_BIT_QUALITY == 0) {
                run("Macro...", "code=[if(v>512 + 1024) {v=-1024;}] stack");
            } else {
                run("Macro...", "code=[if(v>512 + 32768) {v=-1024;}] stack");
            }
            //            run("Gaussian Blur...", "sigma=3 stack"); //OPTIONALTODO?

            // setOption("BlackBackground", false);
            // run("Convert to Mask", "method=Default background=Light calculate");
            run("Min...", "-1024 stack");

            run("Make Binary", "method=Default background=Default calculate");
            run("Auto Threshold", "method=Default black stack use_stack_histogram");

            run("Erode (3D)", "iso=255");
            run("Dilate (3D)", "iso=255");

            //            printLog(LogFile, "*Saving result-image to disk...");
            //            run("Bio-Formats Exporter", "save=" + root_dir + image_output_dir + "BINARY_DOUBLY_THRESHOLDED" + ".tif" + " write_each_z_section compression=Uncompressed");

            //          run("Duplicate...", "duplicate");
            //            binary_doubly_title = getTitle() + "_BINARY_DOUBLY";
            //        rename(binary_doubly_title);
            //            run("Close");
            //      selectWindow(binary_doubly_title);
            //    setLocation(0, 0);
            //  zoomout();
            //zoomout();
            setBatchMode("exit and display");

            selectWindow(dcm_dir); ////////

            run("Original Scale");
            zoomout();
            setLocation(450, 150);

            /*  setBatchMode("hide");
                        run("Gaussian Blur...", "sigma=3 stack");
              setBatchMode("exit and display");
            */

            //Invert and handle cases where it is buggy
            //            if (getPixel(0, 0) == 0 || getPixel(0, getWidth()) == 0 || getPixel(getWidth(), getHeight()) == 0) {
            //          }else{
            setBatchMode("hide");
            run("Grays");

            Stack.setSlice(slices);
            run("Measure");
            name = "Results"; //arrangeAndSessionize("Results", session);
            meanlast = parseFloat(getResult("Mean"));
            if (meanlast < 127) {
                run("Invert", "stack");
            }
            closeWin(name);
            Stack.setSlice(1);
            setBatchMode("exit and display");

            //		selectWindow(binary_doubly_title); zoomout(); zoomout();
            selectWindow(dcm_dir + "-1");
            zoomout();
            zoomout();
            selectWindow(dcm_dir);
            zoomout();
            selectWindow(the_title);
            zoomout();

//	closeWin(dcm_dir + "-2");

detected_top_lobe_slice=0;
detected_bottom_lobe_slice=0;
    //lobes stats:
    printLog(LogFile, " -Cleared outside lobe: " + clearedoutside);
    printLog(LogFile, " -Cleared all: " + clearedall);
    printLog(LogFile, "-Total slices: " + slices);
    printLog(LogFile, "-Detected top lobe slice: " + detected_top_lobe_slice);
    printLog(LogFile, "-Detected bottom lobe slice: " + detected_bottom_lobe_slice);

    //           run("Duplicate...", "duplicate range="+detected_top_lobe_slice+"-"+detected_bottom_lobe_slice);			
    //			selectWindow(getTitle());




    //            run("Subtract...", -1024);


//		run("Concatenate...", "  title=[Concatenated Stacks_"+session+"] image1="+dcm_dir+"-2" +" image2="+dcm_dir+"-3");


    if (clearedall != slices) {
        printLog(LogFile, "-Making stack out of detected lobes only...");
        run("Duplicate...", "duplicate range=" + detected_top_lobe_slice + "-" + slices); //modified endpoint because of bug
        selectWindow(getTitle());
        duptitle = getTitle();
        selectWindow(duptitle);
    }    

			selectWindow(dcm_dir + "-2");

    run("Duplicate...", "duplicate");
    lobes_only_title = getTitle() + "_LOBES_ONLY";
    rename(lobes_only_title);
	closeWin(dcm_dir + "-2");
	closeWin(dcm_dir + "-2");


		printLog(LogFile, "*Lobes approximate area: "+ Lobes_Aprx_Area);


    printLog(LogFile, "Overall time: " + (getTime() - start2) / 1000);
    setBatchMode("exit and display");


    while (nImages > 0) {
        selectImage(nImages);
        close();
    }

/*
		File.close(LFile);
		temp_file = root_dir + dcm_dir + "\\lobes_aprx.txt";
		t_file = File.open(temp_file);
		File.append(""+Lobes_Aprx_Area, t_file);
		File.close(t_file);
		LFile = File.open(LogFile);
*/

    //}
    }
  }
}
setBatchMode("exit and display");


//open("C:\\Users\\Imad\\Desktop\\savelog.png");

//selectWindow("Log");
//save("Log", root_dir + "Log.txt");
File.close(LogFile);
File.close(LFile);

//run("Image Sequence...", "open=C:\\Users\\Imad\\Desktop\\test\\CANCER_da821546432756d377777d7f4c41ca2f_1_256_128_49\\CANCER_da821546432756d377777d7f4c41ca2f__s-1_x-0_y-0_idx-1.tif file=idx");



}



			
