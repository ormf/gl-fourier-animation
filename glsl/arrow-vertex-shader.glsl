// program for indexed drawing: location = 0 contains the vertexes of
// one arrow (precalculated once in the display-window :before
// method), location = 1 contain the offsets/length/angle for each
// arrow to draw, calculated in realtime for each frame in the
// glut:display routine:
// 
// aTransform.xyz = offset of shape
// aTransform.w = length
// aAngle = angle

#version 330 core

layout (location = 0) in vec2 aPos; // The vbo of the shape to draw
layout (location = 1) in vec4 aOffsetLength;  // The offsets from offset buffer
layout (location = 2) in float aAngle;  // The angle

uniform mat4 projection;
// uniform mat4 view;
// uniform mat4 model;

mat2 rotate2d(float _angle){
    return mat2(cos(_angle),-sin(_angle),
                sin(_angle),cos(_angle));
}

void
main()
{
    gl_Position = projection * vec4((aOffsetLength.w*aPos*rotate2d(aAngle))+aOffsetLength.xy, aOffsetLength.z, 1.0);
}
