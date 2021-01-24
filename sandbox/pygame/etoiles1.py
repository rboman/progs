#! /usr/bin/env python3
# -*- coding: utf-8 -*-

# pygame version

from random import randrange

import pygame


BLACK = [0, 0, 0]
WHITE = [255, 255, 255]


class Star:
    def __init__(self, x, y, plan):
        self.x = x
        self.y = y
        self.plan = plan

    def display(self, screen):
        self.x -= self.plan+1
        if self.x < 0:
            self.x = screen.get_width()
            self.y = randrange(screen.get_height())
            self.plan = randrange(8)
        pygame.draw.circle(screen, WHITE, (self.x, self.y), self.plan)


class Framerate:
    """ display the framerate at the bottom of the screen
    """
    def __init__(self):
        self.font = pygame.font.Font(None, 36)

    def display(self, screen, t):
        if t > 0:
            fps = int(1/t*1000)
        else:
            fps = 'inf'
        fps_text = self.font.render(f"{fps} fps", 1, (255, 255, 0))
        pos = fps_text.get_rect()
        pos.left = screen.get_rect().left
        pos.bottom = screen.get_rect().bottom
        screen.blit(fps_text, pos)


class Game:
    def __init__(self, nstars=500):

        size = [320*4, 200*4]
        self.screen = pygame.display.set_mode(size)

        self.stars = []
        for i in range(nstars):
            self.stars.append(
                Star(randrange(size[0]), randrange(size[1]), randrange(8)))

        self.fps = Framerate()

    def run(self):
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

            self.screen.fill(BLACK)

            for s in self.stars:
                s.display(self.screen)

            self.fps.display(self.screen, t)

            # screen everything
            pygame.display.flip()


if __name__ == "__main__":
    pygame.init()
    game = Game()
    game.run()
