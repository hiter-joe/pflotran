"""
Description: Reads an implicit unstructured grid and ensures that associated
             regions are properly set up.
Author: Glenn Hammond
"""
import sys
import os
import traceback

try:
  pflotran_dir = os.environ['PFLOTRAN_DIR']
except KeyError:
    print('PFLOTRAN_DIR must point to PFLOTRAN installation directory and '+
          'be defined in system environment variables.')
    sys.exit(1)
sys.path.append(pflotran_dir + '/src/python')

from unstructured_grid.implicit_unstructured_grid_classes import \
    Grid, SideSet, read_regions_from_file
from common.output import Output

def main():
    
    num_arguments = len(sys.argv)
    if num_arguments < 2:
        sys.exit('ERROR: Please enter an unstructured grid filename:\n  '+
                 'python {} <filename>'.format(os.path.basename(__file__)))
    
    output = Output(filename='parse.stdout')
    
    status = 0
    filename = sys.argv[1]

    grid = Grid(filename)
    grid.read_grid(output)
    grid.cross_reference(output)
    if grid.get_num_cells() < 100:
        grid.print_cross_reference(output)

    list_of_filenames = [filename] + sys.argv[2:num_arguments]
    sideset_list = []    
#    for filename in sys.argv[2:num_arguments]:
    for filename in list_of_filenames:
        output.print_and_return('')
        regions = read_regions_from_file(output,filename)
        if len(regions) > 0:
            sideset_list = sideset_list + regions
    output.print_and_return('')
        
    for sideset in sideset_list:
        sideset.cross_reference(output,grid.get_cells(),grid.get_vertices())
        sideset.print_info(output)
        sideset.print_faces(output,grid.get_cells())
    
    output.destroy()
    return status
    
if __name__ == "__main__":
    try:
        status = main()
        print("success")
    except Exception as error:
        print(str(error))
#        if cmdl_options.backtrace:
#            traceback.print_exc()
        traceback.print_exc()
        print("failure")
        sys.exit(1)
