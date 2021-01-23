#! /usr/bin/env python3
# -*- coding: utf-8 -*-

import pygame
from pygame.locals import *

def main():
    # Initialise screen
    pygame.init()
    screen = pygame.display.set_mode((150, 50))
    pygame.display.set_caption('Basic Pygame program')

    # Fill background
    background = pygame.Surface(screen.get_size())  # create a surface
    background = background.convert()   # convert to the most efficient format for the following
    background.fill((250, 250, 250))  # fill the surface with white

    # Display some text
    font = pygame.font.Font(None, 36) # select a font
    text = font.render("Hello There", 1, (10, 10, 10)) # creates a text object (1=antialisased)
    textpos = text.get_rect()
    textpos.centerx = background.get_rect().centerx # set the centre of the text to the centre of the background
    background.blit(text, textpos)

    # Blit everything to the screen
    screen.blit(background, (0, 0))
    pygame.display.flip()

    # Event loop
    while 1:
        for event in pygame.event.get():
            if event.type == QUIT:
                return

        screen.blit(background, (0, 0))
        pygame.display.flip()


if __name__ == '__main__': main()
