#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://fr.wikibooks.org/wiki/Pygame/Introduction_%C3%A0_Pygame

# https://www.pygame.org/docs/ref/event.html

import sys
import time
import pygame
from pygame.locals import *  # imports K_LEFT, K_RIGHT into the main namespace

# print(dir()) # lots of things!


def main():
    pygame.init()

    screen = pygame.display.set_mode((800, 600), RESIZABLE)

    ball = pygame.image.load("ball.gif").convert_alpha()

    # pygame.display.set_icon(ball) # marche pas

    # the position of the ball is tracked with a Rect
    ballrect = ball.get_rect() # initialised as the Rect of the image

    ballspeed = 3

    speed = [0, 0]
    kpressed = {}

    # game loop
    clock = pygame.time.Clock()
    
    pygame.event.set_blocked(MOUSEMOTION) # don't capture MouseMotion

    while 1:

        # loop type 1 => holding an arrow key does not produce any event 
        #                (unless pygame.key.set_repeat is set)
        #             => a key produces TextEditing event

        if 0:
            for event in pygame.event.get():
                print(f"{pygame.event.event_name(event.type)} ({event.type})")
                if event.type == pygame.QUIT:
                    return
                elif event.type == KEYDOWN:
                    # print('KEYDOWN')
                    if event.key == K_q or event.key == K_LEFT:
                        speed[0] = -ballspeed
                        print("left")
                    if event.key == K_d or event.key == K_RIGHT:
                        speed[0] = ballspeed
                        print("right")
                    if event.key == K_z or event.key == K_UP:
                        speed[1] = -ballspeed
                        print("up")
                    if event.key == K_s or event.key == K_DOWN:
                        speed[1] = ballspeed
                        print("down")
                elif event.type == KEYUP:
                    # print('KEYUP')
                    if event.key in [K_q, K_d, K_LEFT, K_RIGHT]:
                        speed[0] = 0
                        print("stop x")
                    if event.key in [K_s, K_z, K_UP, K_DOWN]:
                        speed[1] = 0
                        print("stop y")
        else:  # loop type 2: we keep the pressed state in a dict
            for event in pygame.event.get():
                print(f"{pygame.event.event_name(event.type)} ({event.type})")
                if event.type == pygame.QUIT:
                    return
                elif event.type == KEYDOWN:
                    kpressed[event.key] = True
                elif event.type == KEYUP:
                    kpressed[event.key] = False

            # set the velocity of the ball along X
            if kpressed.get(K_q) or kpressed.get(K_LEFT):
                speed[0] = -ballspeed
            elif kpressed.get(K_d) or kpressed.get(K_RIGHT):
                speed[0] = ballspeed
            else:
                speed[0] = 0

            # set the velocity of the ball along Y
            if kpressed.get(K_z) or kpressed.get(K_UP):
                speed[1] = -ballspeed
            elif kpressed.get(K_s) or kpressed.get(K_DOWN):
                speed[1] = ballspeed
            else:
                speed[1] = 0

        # ballrect = ballrect.move(speed) # move the rect
        ballrect.move_ip(speed) # move the rect "in place"
        ballrect.clamp_ip(screen.get_rect()) # clamp "in place" the ball rect inside the screen rect.

        # clears the screen
        screen.fill((0, 0, 50))

        screen.blit(ball, ballrect)
        pygame.draw.rect(screen, (255, 255, 255), ballrect,
                         1)  # display the bounding rect

        # time.sleep(0.01)
        pygame.display.flip()
        clock.tick(60)


if __name__ == "__main__":
    main()
