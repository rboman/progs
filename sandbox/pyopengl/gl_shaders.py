#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://robertvandeneynde.be/

from OpenGL.GL import *
from OpenGL.GL import shaders
import ctypes
import pygame

from vecutils import * 

pygame.init()

vertex_shader = """
#version 330

in vec4 position;

void main() {
    gl_Position = position;
}
"""

fragment_shader = """
#version 330

out vec4 pixel;

void main() {
    pixel = vec4(1, 0.5, 0, 1);
}
"""

vertices = farray([
    0.6, 0.6, 0, 1.0,
    -0.6, 0.6, 0, 1.0,
    0.0, -0.6, 0, 1.0,
])

pygame.display.set_mode((512, 512), pygame.OPENGL | pygame.DOUBLEBUF)
glClearColor(0.9, 0.9, 0.5, 1.0)

shader_program = shaders.compileProgram(
    shaders.compileShader(vertex_shader, GL_VERTEX_SHADER),
    shaders.compileShader(fragment_shader, GL_FRAGMENT_SHADER))

# Create a new VAO (Vertex Array Object) and bind it
vertex_array_object = glGenVertexArrays(1)
glBindVertexArray(vertex_array_object)

# Get the position of the 'position' in parameter of our shader_program and bind it.
position = glGetAttribLocation(shader_program, 'position')

if position != -1: # maybe the attribute is useless and was discarded by the compiler
    glEnableVertexAttribArray(position)

    # Generate buffers to hold our vertices
    vertex_buffer = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer)

    # Describe the position data layout in the buffer
    glVertexAttribPointer(position, 4, GL_FLOAT, False, 0, ctypes.c_void_p(0))

    # Send the data over to the buffer
    glBufferData(GL_ARRAY_BUFFER, 48, vertices, GL_STATIC_DRAW) # 48 bytes = ArrayDatatype.arrayByteCount(vertices)
else:
    print('Inactive attribute "{}"'.format('position'))

# Unbind the VAO
glBindVertexArray(0)

# Unbind the VBO
glBindBuffer(GL_ARRAY_BUFFER, 0)

clock = pygame.time.Clock()

done = False
while not done:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            done = True
    
    # tick
    
    # draw
    glClear(GL_COLOR_BUFFER_BIT)
    glUseProgram(shader_program)
    
    glBindVertexArray(vertex_array_object)
    glDrawArrays(GL_TRIANGLES, 0, 3) # 3 points = len(vertices) // 4
    glBindVertexArray(0)
    
    glUseProgram(0)
    
    pygame.display.flip()
    clock.tick(60)

pygame.quit()
