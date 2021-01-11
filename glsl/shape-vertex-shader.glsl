// program for drawing the shape: location = 0 contain the 3D
// vertexes of the shape, calculated in realtime for each frame in
// the glut:display routine.

#version 330 core

layout (location = 0) in vec3 aPos;

uniform mat4 projection;

void main()
{
    gl_Position = projection * vec4(aPos.xyz, 1.0);
}
