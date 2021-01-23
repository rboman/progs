#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# pygame version

from random import randrange

import pygame
pygame.init()

size = [320*4, 200*4]
screen = pygame.display.set_mode(size)

BLACK = [0, 0, 0]
WHITE = [255, 255, 255]
NSTARS = 500

class Star:
    def __init__(self, x, y, plan):
        self.x = x
        self.y = y
        self.plan = plan

stars = []
for i in range(NSTARS):
    stars.append(Star(randrange(size[0]), randrange(size[1]), randrange(8)))

font = pygame.font.Font(None, 36)
clock = pygame.time.Clock()

stop = False
while not stop:
    
    # t = clock.tick()
    t = clock.tick(120)
    # t = clock.tick_busy_loop(120)

    # events
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            stop = True 

    screen.fill(BLACK)
    
    for s in stars:
        s.x -= s.plan+1
        if s.x < 0:
            s.x = size[0]
            s.y = randrange(size[1])
            s.plan = randrange(8)
        pygame.draw.circle(screen, WHITE, (s.x, s.y), s.plan)


    # display screen framerate
    if t>0:
        fps = int(1/t*1000)
    else:
        fps = 'inf'
    fps_text = font.render(f"{fps} fps", 1, (255, 255, 0))
    pos = fps_text.get_rect()
    pos.left = screen.get_rect().left
    pos.bottom = screen.get_rect().bottom
    screen.blit(fps_text, pos)

    # screen everything
    pygame.display.flip()

    
pygame.quit()
