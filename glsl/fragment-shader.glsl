#version 330 core

out vec4 fColor;

uniform vec4 Color = vec4(1.0, 1.0, 1.0, 1.0);

void main()
{
    fColor = Color;
}
