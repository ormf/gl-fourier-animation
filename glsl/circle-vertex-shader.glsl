// program for indexed drawing: location = 0 contain the vertexes of
// one circle (precalculated once in the display-window :before
// method), location = 1 contain the offsets/length/angle for each
// circle to draw, calculated in realtime for each frame in the
// glut:display routine:
// 
// aTransform.xy = offset of shape
// aTransform.w = angle
// aTransform.z = length
// 
// The phase (aTransform.w) isn't used as the circles don't have to
// be rotated.

#version 330 core

layout (location = 0) in vec2 aPos; // The vbo of the shape to draw
layout (location = 1) in vec4 aOffsetLength;  // The offsets from offset buffer
// layout (location = 2) in float aAngle;  // The angle (unused here)

uniform mat4 projection;
// uniform mat4 view;
// uniform mat4 model;

void
main()
{
    gl_Position = projection * vec4((aOffsetLength.w*aPos)+aOffsetLength.xy, aOffsetLength.z, 1.0);
}
