# Python script to convert SLATS data into a consistent set of rasters
# Arguments
# 0. in_file: in file path
# 1. out_file: output file path
# 2. field: field to preserve in the shapefile


import arcpy
from arcpy import env
from arcpy.sa import *
import sys
import os
from pathlib import Path

env.overwriteOutput = True

dirname = os.path.dirname(__file__)

ref_raster = os.path.join(dirname, '../../data/NSW_LGA_GDA2020.tif')

out_coor_system = arcpy.Describe(ref_raster).spatialReference

env.extent = ref_raster
env.cellSize = 5

env.workspace = os.path.join(dirname, '../../processed_data/slats')
env.scratchWorkspace = os.path.join(dirname, '../../processed_data/slats')

recreateFile = True

def __main__():
    in_file = sys.argv[1]
    out_file = sys.argv[2]

    file_name, file_ext = os.path.splitext(in_file)

    # Create scratch GDB
    #out_file = sys.argv[2]
    #out_file_name, a = os.path.splitext(out_file)
    #if not os.path.exists(os.path.join(dirname, '../../processed_data/slats',out_file_name)):
    #    os.mkdir(os.path.join(dirname, '../../processed_data/slats',out_file_name))
    #if not arcpy.Exists(os.path.join(dirname, '../../processed_data/slats',out_file_name,'/scratch.gdb')):
    #    arcpy.management.CreateFileGDB(os.path.join(dirname, '../../processed_data/slats', out_file_name), "scratch.gdb")

    if (file_ext) == ".shp":
        rasterizeShp(in_file, out_file)
    else:
        reprojectRaster(in_file, out_file)

def append_ext(original_path, append, ext = None):
    # Extract the base name and current extension
    base_name, extension = os.path.splitext(os.path.basename(original_path))

    # Append your desired string
    new_base_name = base_name + '_' + append

    if (ext is not None):
        extension = ext

    # Construct the new path with the same extension
    new_path = os.path.join(os.path.dirname(original_path), new_base_name + extension)

    return new_path

def rasterizeShp(in_file, out_file):
    head, tail = os.path.split(in_file)
    field = sys.argv[3]

    if not os.path.isfile(in_file) or recreateFile:
        print("1. Projecting {}...\n".format(tail))
        temp_projected_shp = append_ext(out_file, "proj", '.shp')
        arcpy.Project_management(in_file, temp_projected_shp, out_coor_system)

        print("2. Converting polygon to raster for {}...\n".format(tail))
        #temp_file = append_ext(out_file, "unclipped")
        arcpy.conversion.PolygonToRaster(temp_projected_shp, field, out_file) # Cell size will be based on the snap raster

        #print("2. Clipping {}...\n".format(tail))
        #arcpy.Clip_management(temp_file, None, out_file, ref_raster)

    print("3. Reclassifying {}...\n".format(tail))
    out_file = reclassifyRaster(out_file)

    print("4. Deleting unclipped version {}\n".format(tail))
    # Delete temporary files
    arcpy.Delete_management(temp_projected_shp)
    #arcpy.Delete_management(temp_file)

    print("Conversion complete for {}, saved in {}".format(tail, out_file))
    return

def reprojectRaster(in_file, out_file):
    head, tail = os.path.split(in_file)
    if not os.path.isfile(in_file) or recreateFile:
        print("1. Projecting {}...\n".format(tail))
        temp_proj_file = append_ext(out_file, "proj")
        arcpy.ProjectRaster_management(in_file, temp_proj_file, ref_raster)

        print("2. Resampling {}...\n".format(tail))
        #temp_file = append_ext(out_file, "unclipped")
        arcpy.Resample_management(temp_proj_file, out_file)
    
    #print("3. Clipping {}...\n".format(tail))
    #arcpy.Clip_management(temp_file, None, out_file, ref_raster)

    print("4. Reclassifying {}...\n".format(tail))
    out_file = reclassifyRaster(out_file)

    print("4. Deleting unclipped version {}\n".format(tail))
    arcpy.Delete_management(temp_proj_file)
    #arcpy.Delete_management(temp_file)

    print("Conversion complete for {}, saved in {}".format(tail, out_file))
    return

def reclassifyRaster(rast_path):
    # Check out extension
    checkOutExt()
    
    out_file = sys.argv[2]
    change_code = sys.argv[4]
    
    rast_path_recode = append_ext(rast_path, "recode")

    if (change_code == "19882008"):
        remapTable = RemapValue([[11,1],[12,1],[40,2],[41,2],[42,2],[55,2],[46,3],[48,3],[51,3],[53,4],[60,4],[61,4],[62,4]])
    elif (change_code == "20082014"):
        remapTable = RemapRange([[10,13,1], [70,79,2], [80,89,3], [90, 99, 4]])
    else:
        # No need for reclassification
        os.rename(rast_path, rast_path_recode)
        return(rast_path_recode)

    # Reclassify
    with arcpy.EnvManager(scratchWorkspace=r"memory"):
        out_raster = arcpy.sa.Reclassify(
            in_raster=rast_path,
            reclass_field="Value",
            remap=remapTable,
            missing_values="NODATA"
        )
    
    # Save that to the original raster path
    out_raster.save(rast_path_recode)

    return(rast_path_recode)

def checkOutExt():
    # Check out spatial analyst license
    class LicenseError(Exception):
        pass

    try:
        if arcpy.CheckExtension("Spatial"):
            arcpy.CheckOutExtension("Spatial")
        else:
            raise LicenseError
        
    except LicenseError:
        print("Spatial Analyst license is unavailable")

if __name__ == "__main__":
    __main__()

