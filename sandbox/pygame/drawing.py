#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# https://www.pygame.org/docs/ref/draw.html

import pygame
from pygame.locals import *


def main():
    pygame.init()

    # print(pygame.font.get_fonts()) # display all the available fonts

    # open window
    screen = pygame.display.set_mode( (800, 600) )
    pygame.display.set_caption(__file__)

    # create a static background
    background = pygame.Surface(screen.get_size())  # create a surface
    background = background.convert()   # convert to the most efficient format for the following
    background.fill((0, 0, 0))  # fill the surface with black
    # draw a white rectangle
    pygame.draw.rect(background, (255, 255, 255), Rect( 10, 10, 100, 50) )
    # draw a red rectangle outline
    pygame.draw.rect(background, (255, 0, 0), Rect( 10, 80, 100, 50), width=2 )
    # polygon
    pygame.draw.polygon(background, (255, 255, 0), [(150,50), (100,200), (250, 0) ])


    # font & draw some text
    font = pygame.font.Font(None, 36) # select a font
    quit_text = font.render("<space> to quit", 1, (200, 200, 200))

    consolas_font = pygame.font.SysFont('consolas', 16) # select consolas font

    pos = quit_text.get_rect() # get a copy of the rect
    pos.centerx = background.get_rect().centerx  # modify this copy
    pos.bottom = background.get_rect().bottom
    background.blit(quit_text, pos)


    # create a surface with a circle
    radius = 50
    circle = pygame.Surface( (2*radius, 2*radius), pygame.SRCALPHA )   # surface with alpha channel
    # circle.set_alpha(100)  # 1 alpha for the whole surface
    pygame.draw.circle(circle, (0, 0, 255, 100), (radius, radius), radius)



    # game loop

    clock = pygame.time.Clock()

    while 1:

        # This limits the while loop to a max of 60 times per second.
        t = clock.tick(60) # time between 2 calls
        # t = clock.tick_busy_loop(60) # time between 2 calls (more accurate than "tick")

        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return
            elif event.type == KEYDOWN:
                if event.key == K_SPACE:
                    return

        # draw background
        screen.blit(background, (0, 0))

        # display framerate
        fps_text = consolas_font.render(f"{int(1/t*1000)} fps", 1, (255, 255, 0))
        pos = fps_text.get_rect()
        pos.left = background.get_rect().left
        pos.bottom = background.get_rect().bottom
        screen.blit(fps_text, pos)

        # mouse position
        mpos = pygame.mouse.get_pos()

        # draw mouse pos
        mpos_text = consolas_font.render(f"{mpos}", 1, (255, 255, 0))
        pos = mpos_text.get_rect()
        pos.right = background.get_rect().right
        screen.blit(mpos_text, pos)

        circle_rect = circle.get_rect()
        # pygame.draw.circle(screen, (0, 0, 255, 100), mpos, 50)
        cpos = (mpos[0]-circle_rect.centerx, mpos[1]-circle_rect.centery)
        screen.blit(circle, cpos)


        # display everything
        pygame.display.flip()



if __name__=="__main__":
    main()
