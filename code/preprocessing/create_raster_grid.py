import arcpy
from arcpy import env
from arcpy.sa import *
import sys
import os

arcpy.CheckOutExtension("Spatial")

dirname = os.path.dirname(__file__)

rast_path = os.path.join(dirname, "../../processed_data/slats/slats_1988_1990.tif")


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

def reclassifyRaster(rast_path):
    # Check out extension
    checkOutExt()

    change_code = "19882008"

    if (change_code == "19882008"):
        remapTable = RemapValue([[11,1],[12,1],[40,2],[41,2],[42,2],[55,2],[46,3],[48,3],[51,3],[53,4],[60,4],[61,4],[62,4]])
    elif (change_code == "20082014"):
        remapTable = RemapRange([[10,13,1], [70,77,2], [80,88,3], [90, 97, 4]])
    else:
        # No need for reclassification
        return(rast_path)
    
    unclass_rast = append_ext(rast_path, "unclass")

    # Copy unclassified raster
    arcpy.Rename_management(rast_path, unclass_rast)

    # Reclassify
    with arcpy.EnvManager(scratchWorkspace=env.scratchWorkspace):
        out_raster = arcpy.sa.Reclassify(
            in_raster=unclass_rast,
            reclass_field="Value",
            remap=remapTable
        )
    
    # Save that to the original raster path
    out_raster.save(rast_path)

    # Delete unclassified raster
    arcpy.Delete_management(unclass_rast)

    return(rast_path)

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

reclassifyRaster(rast_path=rast_path)