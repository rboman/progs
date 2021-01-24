#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://fr.wikibooks.org/wiki/Pygame/Introduction_%C3%A0_Pygame

# https://www.pygame.org/docs/ref/event.html

import sys
import time
import pygame
from pygame.locals import * # imports K_LEFT, K_RIGHT into the main namespace

# print(dir())

def main():
    pygame.init()

    screen = pygame.display.set_mode( (800, 600) )

    ball = pygame.image.load("ball.gif")
    ballrect = ball.get_rect()

    speed = [0, 0]
    kpressed = {}

    # game loop
    clock = pygame.time.Clock()
    while 1:

        # loop type 1 => holding a key does not produce any event
        for event in pygame.event.get():
            print(f"{pygame.event.event_name(event.type)} ({event.type})")
            if event.type == pygame.QUIT:
                sys.exit()
            elif event.type == KEYDOWN:
                # print('KEYDOWN')
                if event.key == K_q or event.key == K_LEFT:
                    speed[0] = -1
                    print("left")
                if event.key == K_d or event.key == K_RIGHT:
                    speed[0] = 1
                    print("right")
                if event.key == K_z or event.key == K_UP:
                    speed[1] = -1
                    print("up")
                if event.key == K_s or event.key == K_DOWN:
                    speed[1] = 1
                    print("down")
            elif event.type == KEYUP:
                # print('KEYUP')
                if event.key in [ K_q, K_d, K_LEFT, K_RIGHT]:
                    speed[0] = 0
                    print("stop x")
                if event.key in [ K_s, K_z, K_UP, K_DOWN]:
                    speed[1] = 0
                    print("stop y")

        ballrect = ballrect.move(speed)

        screen.fill( (0,0,0) )
        screen.blit(ball, ballrect)
        pygame.draw.rect(screen, (255,255,255), ballrect, 1) # display bounding rect
        #time.sleep(0.01)
        pygame.display.flip()
        clock.tick(60)



if __name__ == "__main__":
    main()