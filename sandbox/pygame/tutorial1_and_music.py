#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# from https://fr.wikibooks.org/wiki/Pygame/Introduction_%C3%A0_Pygame
# and https://nerdparadise.com/programming/pygame/part3

# music from Patrick de Arteaga: 
#   https://patrickdearteaga.com/chiptune-8-bit-retro/
# bounce sound from https://freesound.org/people/bumpelsnake/sounds/456567/


import sys, time, pygame
pygame.init()

size = width, height = 640, 480
speed = [4, 4]
black = 0, 0, 0

screen = pygame.display.set_mode(size)

pygame.mixer.music.load('Common_Fight.ogg')
pygame.mixer.music.play(-1) # 0= play once; -1: infinite
effect = pygame.mixer.Sound('bounce.ogg')
effect.set_volume(0.4)  # between 0 and 1

ball = pygame.image.load("ball.gif")
ballrect = ball.get_rect()

while 1:
    for event in pygame.event.get():
        if event.type == pygame.QUIT: sys.exit()

    ballrect = ballrect.move(speed)
    if ballrect.left < 0 or ballrect.right > width:
        speed[0] = -speed[0]
        effect.play()
    if ballrect.top < 0 or ballrect.bottom > height:
        speed[1] = -speed[1]
        effect.play()
    screen.fill(black)
    screen.blit(ball, ballrect)
    time.sleep(0.01)
    pygame.display.flip()

