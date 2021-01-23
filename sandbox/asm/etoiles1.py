#! /usr/bin/env python3
# -*- coding: utf-8 -*-

from random import randrange

import pygame
pygame.init()

size = [320*4, 200*4]
display = pygame.display.set_mode(size)

BLACK = [0, 0, 0]
WHITE = [255, 255, 255]

class Star:
    def __init__(self, x, y, plan):
        self.x = x
        self.y = y
        self.plan = plan

stars = []
for i in range(500):
    stars.append(Star(randrange(size[0]), randrange(size[1]), randrange(8)))

clock = pygame.time.Clock()

stop = False
while not stop:
    
    # events
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            stop = True 

    display.fill(BLACK)
    
    for s in stars:
        s.x -= s.plan+1
        if s.x < 0:
            s.x = size[0]
            s.y = randrange(size[1])
            s.plan = randrange(8)
        pygame.draw.circle(display, WHITE, (s.x, s.y), s.plan)

    pygame.display.flip()
    clock.tick(60)
    
pygame.quit()
