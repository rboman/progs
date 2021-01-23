#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://fr.wikibooks.org/wiki/Pygame/Introduction_%C3%A0_Pygame

import sys
import time
import pygame
from pygame.locals import *
pygame.init()

size = width, height = 320, 240
speed = [0, 0]
black = 0, 0, 0

screen = pygame.display.set_mode(size)

ball = pygame.image.load("intro_ball.gif")
ballrect = ball.get_rect()

clock = pygame.time.Clock()

while 1:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == KEYDOWN:
            if event.key == K_q or event.key == K_LEFT:
                speed[0] = -1
            if event.key == K_d or event.key == K_RIGHT:
                speed[0] = 1
            if event.key == K_z or event.key == K_UP:
                speed[1] = -1
            if event.key == K_s or event.key == K_DOWN:
                speed[1] = 1
        elif event.type == KEYUP:
            if event.key in [ K_q, K_d, K_LEFT, K_RIGHT]:
                speed[0] = 0
            if event.key in [ K_s, K_z, K_UP, K_DOWN]:
                speed[1] = 0

    ballrect = ballrect.move(speed)

    screen.fill(black)
    screen.blit(ball, ballrect)
    pygame.draw.rect(screen, (255,255,255), ballrect, 1) # display bounding rect
    #time.sleep(0.01)
    pygame.display.flip()
    clock.tick(60)