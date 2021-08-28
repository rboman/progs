#ifndef TILES_H
#define TILES_H

#include "dungeon.h"
#include "Game.h"
#include "Tile.h"

/// This object stores all the tiles and sprites
/// as a map "tile_name" => "Tile object"
///
/// This objects also acts as a "Scene" (inspect the tiles)
/// TODO: extract a TileInspectorScene from this object

class Tiles
{
    /// scene-related data
    float atime;            ///< animation counter
    std::string message;    ///< message to be displayed at the bottom of the screen

public:
    std::unique_ptr<olc::Sprite> tileimg; ///< sprite/tile image
    std::unique_ptr<olc::Decal> decal;    ///< sprite/tile decal
    std::map<std::string, Tile> tilemap;  ///< map of tiles

    Tiles();

    void load();

    /// draw the tiles
    void update(olc::PixelGameEngine &pge, float fElapsedTime);
    
    void print(std::ostream &out=std::cout);
};

#endif // TILES_H
