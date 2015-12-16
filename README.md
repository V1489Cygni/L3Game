# L3Game
3D Labyrinth game.

Move through the labyrinth from the start point (green cube) to the finish one
(red). You can move forward/backward and left/right but not up and down (you
will fall if there is nothing under you). You can choose a direction to lead
down and continue you way, navigating through the remaining dimensions.
Obviously, you can't move through the walls and boxes (gray cubes). Boxes are
falling down according to the current gravity vector. They move one unit down,
each time you make a move, but you've got higher priority to occupy a cell.
Note that you can't change the gravity vector while you are falling.

Controls.

Camera:
Ctrl + [Arrow Up, Arrow Down, Arrow Left, Arrow Right, '+', '-'] - Move camera.
Ctrl + 'i' - Set camera into it's initial state.
Ctrl + 'f' - Toggle filter higher walls.
Ctrl + 'g' - Toggle filter all walls.

Game:
Game expects level filename as an argument.
['x', 'y', 'z']               - Set the corresponding axis to lead down.
['X', 'Y', 'Z']               - Set the corresponding axis to lead up.
Arrow Keys                    - Move through the level.
['w', 'a', s', 'd', '<', '>'] - Move point of view.
Space                         - Wait one step (you may need this to wait for a box to fall).
Ctrl + 'r'                    - Restart level.
Ctrl + 'q'                    - Quit.

Editor:
Editor expects level filename as an argument.
Ctrl + ['x', 'y', 'z']         - Set the corresponding axis to lead down.
Ctrl + ['X', 'Y', 'Z']         - Set the corresopnfing axis to lead up.
Arrow Keys, '<', '>'           - Move through the level.
['x', 'y', 'z', 'X', 'Y', 'Z'] - Toggle a wall in the corresponding direction.
's'                            - Set here a start point.
'f'                            - Set here a finish point.
'b'                            - Toggle here a box.
Ctrl + 's'                     - Save level.
Ctrl + 'q'                     - Quit.
