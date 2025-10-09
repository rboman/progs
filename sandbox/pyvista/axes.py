"""Show a translucent cube with origin-centered axes.

Usage:
  python axes.py            # interactive window (requires PyVista and a GUI backend)
  python axes.py --offscreen  # render off-screen and save 'axes.png'
"""

import sys
import argparse

try:
    import pyvista as pv
    import numpy as np
except Exception as e:
    print("Error: PyVista (and dependencies) are required to run this script.")
    print("Install with (from a Windows cmd shell):")
    print(r"  py -m venv .venv && .venv\\Scripts\\activate && py -m pip install -U pip")
    print(r"  pip install pyvista pyvistaqt vtk PyQt5")
    print()
    print("After installation, run: python axes.py")
    raise SystemExit(1)

# Create a cube mesh
mesh = pv.Cube()

# Function to add axes centered at the origin (0,0,0)
def add_origin_axes(plotter, mesh, length=None, line_width=5, label_size=16):
    """Add simple X/Y/Z axes centered at the origin.

    - length: half-length of each axis (from -length to +length). If None, computed
      from the scene mesh bounds.
    - line_width: thickness of axis lines.
    - label_size: font size for the axis labels.
    """
    # Determine a reasonable length from the mesh bounds if not provided
    if length is None:
        b = mesh.bounds  # (xmin,xmax,ymin,ymax,zmin,zmax)
        max_extent = max(b[1] - b[0], b[3] - b[2], b[5] - b[4])
        length = max_extent * 0.7 if max_extent and max_extent > 0 else 1.0

    # Create lines for the three axes
    x_axis = pv.Line([-length, 0.0, 0.0], [length, 0.0, 0.0])
    y_axis = pv.Line([0.0, -length, 0.0], [0.0, length, 0.0])
    z_axis = pv.Line([0.0, 0.0, -length], [0.0, 0.0, length])

    # Add them as opaque colored lines so they are visible even if the cube is translucent
    plotter.add_mesh(x_axis, color='red', line_width=line_width, name='origin_x_axis')
    plotter.add_mesh(y_axis, color='green', line_width=line_width, name='origin_y_axis')
    plotter.add_mesh(z_axis, color='blue', line_width=line_width, name='origin_z_axis')

    # Add labels slightly beyond the positive ends
    label_pos = np.array([[length * 1.05, 0.0, 0.0], [0.0, length * 1.05, 0.0], [0.0, 0.0, length * 1.05]])
    plotter.add_point_labels(label_pos, ['X', 'Y', 'Z'], font_size=label_size, text_color='black', point_size=0, name='origin_axis_labels')


def build_scene(off_screen=False):
    """Create the plotter and add cube + origin axes. Return the Plotter instance."""
    pl = pv.Plotter(off_screen=off_screen)

    # Add the cube as a translucent mesh (opacity between 0.0 and 1.0)
    cube_actor = pl.add_mesh(mesh, color='lightgray', opacity=0.4, smooth_shading=True)

    # Add cube axes actor (visual bounding axes)
    cube_axes_actor = pv.CubeAxesActor(pl.camera)
    cube_axes_actor.bounds = mesh.bounds
    pl.add_actor(cube_axes_actor)

    # Add origin-centered axes
    add_origin_axes(pl, mesh)

    return pl


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument('--offscreen', action='store_true', help='Render off-screen and save a PNG (axes.png)')
    args = parser.parse_args(argv)

    pl = build_scene(off_screen=args.offscreen)

    if args.offscreen:
        # Render off-screen and save a screenshot
        out = 'axes.png'
        # show() with off_screen True does not open a window; use screenshot to save
        pl.show(auto_close=False)
        pl.screenshot(out)
        pl.close()
        print(f"Saved off-screen render to {out}")
    else:
        # Interactive window (requires GUI)
        pl.show()


if __name__ == '__main__':
    main()
