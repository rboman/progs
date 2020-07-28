// from https://github.com/Moros1138/PGEAsteroids
//
// build & run
// mkdir build
// cmake -A x64 ..
// cmake --build . --config Release && Release\asteroids.exe

#include "config.h"

#define OLC_PGE_APPLICATION
#include "olcPixelGameEngine.h"

#define OLC_PGEX_PARTICLES_APP
#include "olcPGEX_Particles.h"

struct sSpaceObject {
	// life cycle data
	bool alive;
	
	// position/movement/orientation/size data
	olc::vf2d pos;
	olc::vf2d origin;
	olc::vf2d vel;
	float angle;
	olc::vf2d scale;

	// drawing
	olc::Sprite *sprite;
	olc::Decal *decal;

	float travel = 0.0f;
	float maxTravel = 0.0f;
	olc::Pixel tint = olc::WHITE;

	void Update(olc::PixelGameEngine &pge)
	{
		olc::vf2d p = this->pos;
		bool x = false, y = false;

		if(p.x < 0.0f)
		{
			p.x += (float)pge.ScreenWidth();
			x = true;
		}				
		
		if(p.x >= (float)pge.ScreenWidth() && !x)
			p.x -= (float)pge.ScreenWidth();


		if(p.y < 0.0f)
		{
			p.y += (float)pge.ScreenHeight();
			y = true;
		}				
		
		if(p.y >= (float)pge.ScreenHeight() && !y)
			p.y -= (float)pge.ScreenHeight();


		this->pos = p;
	}

	void Draw(olc::PixelGameEngine &pge)
	{
		olc::vf2d p = this->pos;
		
		bool x = false, y = false;
		
		if(this->pos.x - ((this->sprite->width * this->scale.x) / 2) < 0.0f)
		{
			p.x += (float)pge.ScreenWidth();
			x = true;
		}

		if(this->pos.x + ((this->sprite->width * this->scale.x) / 2) >= (float)pge.ScreenWidth() && !x)
			p.x -= (float)pge.ScreenWidth();
		
		if(this->pos.y - ((this->sprite->height * this->scale.y) / 2) < 0.0f)
		{
			p.y += (float)pge.ScreenHeight();
			y = true;
		}

		if(this->pos.y + ((this->sprite->height * this->scale.y) / 2) >= (float)pge.ScreenHeight() && !y)
			p.y -= (float)pge.ScreenHeight();

		if(p.x != this->pos.x || p.y != this->pos.y)
			pge.DrawRotatedDecal(p, this->decal, this->angle, this->origin, this->scale, this->tint);
		
		pge.DrawRotatedDecal(this->pos, this->decal, this->angle, this->origin, this->scale, this->tint);
	}
};


class PGE_Asteroids : public olc::PixelGameEngine
{
public:
	PGE_Asteroids()
	{
		sAppName = "PGEAsteroids";
	} 

public:
	
	bool OnUserCreate() override
	{
		spriteBackground = new olc::Sprite(std::string(CMAKE_SOURCE_DIR) + "/assets/nebula/stars.png");
		
		// Asteroid
		spriteAsteroid = new olc::Sprite(std::string(CMAKE_SOURCE_DIR) + "/assets/meteorBrown_big4.png");
		decalAsteroid = new olc::Decal(spriteAsteroid);

		// Bullet
		spriteBullet = new olc::Sprite(std::string(CMAKE_SOURCE_DIR) + "/assets/laserBlue01.png");
		decalBullet = new olc::Decal(spriteBullet);
		
		// Ship
		spriteShip = new olc::Sprite(std::string(CMAKE_SOURCE_DIR) + "/assets/playerShip1_red.png");
		decalShip = new olc::Decal(spriteShip);
		
		// Draw Stuff
		Clear(olc::BLACK);
		for(int y = 0; y < ScreenHeight(); y += spriteBackground->height)
		{
			for(int x = 0; x < ScreenWidth(); x += spriteBackground->width)
			{
				DrawSprite(x, y, spriteBackground);
			}
		}

		explosions = new olc::Particles(std::string(CMAKE_SOURCE_DIR) + "/assets/blackSmoke17.png", olc::ParticleAnimations::StarBurstCreate, olc::ParticleAnimations::StarBurstMove, {0.7f, 0.7f});
		explosions2 = new olc::Particles(std::string(CMAKE_SOURCE_DIR) + "/assets/meteorBrown_big4.png", olc::ParticleAnimations::StarBurstCreate, olc::ParticleAnimations::StarBurstMove, {0.2f, 0.2f});

		ResetGame();
		return true;
	}
	
	bool OnUserUpdate(float fElapsedTime) override
	{
		if(!player.alive || GetKey(olc::R).bPressed)
			ResetGame();
		
		if(GetKey(olc::ESCAPE).bPressed)
			return false;

		if(GetKey(olc::LEFT).bHeld)
			player.angle -= 3.0f * fElapsedTime;

		if(GetKey(olc::RIGHT).bHeld)
			player.angle += 3.0f * fElapsedTime;
		
		if(GetKey(olc::UP).bHeld)
		{
			player.vel.x += sinf(player.angle) * 50.0f * fElapsedTime;
			player.vel.y += -cosf(player.angle) * 50.0f * fElapsedTime;
		}

		// player movement
		player.pos += player.vel * fElapsedTime;
		player.Update(*this);

		if(GetKey(olc::SPACE).bReleased /*&& listBullets.size() < 5*/)
		{
			listBullets.push_back({
				true, // alive
				{ player.pos.x, player.pos.y }, // pos
				{ (float)spriteBullet->width / 2, (float)spriteBullet->height  / 2 }, // origin
				{ 150.0f * sinf(player.angle), -150.0f * cosf(player.angle) }, // vel
				player.angle, // angle
				{ 0.8f, 0.8f }, // scale
				spriteBullet, // sprite
				decalBullet, // decal
				0.0f,
				400.0f
			});
		}
		
		// asteroid movement
		for(auto &a : listAsteroids)
		{
			if(a.alive)
			{
				a.pos += a.vel * fElapsedTime;
				a.Update(*this);
				
				if(a.vel.x > 0.0f)
					a.angle += 1.0f * fElapsedTime;
				else
					a.angle -= 1.0f * fElapsedTime;
			
				olc::vf2d p;
				olc::vf2d aSize= {
					(a.sprite->width / 2) * a.scale.x * 0.8f,
					(a.sprite->height / 2) * a.scale.y * 0.8f
				};

				p = player.pos - a.pos;
				if(p.mag() < aSize.mag())
				{
					explosions->Spawn(player.pos);
					explosions2->Spawn(player.pos);
					player.alive = false;
				}
			
			}
		}

		// bullet movement
		for(auto &b : listBullets)
		{
			if(b.alive)
			{
				b.pos += b.vel *fElapsedTime;
				b.Update(*this);
				
				b.travel += b.vel.mag() * fElapsedTime;
				
				if(b.travel > b.maxTravel)
				{
					b.alive = false;
					continue;
				}

				for(auto &a : listAsteroids)
				{
					if(a.alive)
					{
						olc::vf2d p;
						olc::vf2d aSize= {
							(a.sprite->width / 2) * a.scale.x * 0.8f,
							(a.sprite->height / 2) * a.scale.y * 0.8f
						};

						p = b.pos - a.pos;
						if(p.mag() < aSize.mag())
						{
							// asteroid died
							a.alive = false;
							nScore += 20;
							
							// bullet died (or comitted suicide, however you wanna frame it)
							b.alive = false;

							a.scale *= 0.7f;
							
							explosions->Spawn(a.pos);
							explosions2->Spawn(a.pos);

							if(a.scale.x > 0.4f)
							{
								float angle1 = ((float)rand() / (float)RAND_MAX) * 6.283185f;
								float angle2 = ((float)rand() / (float)RAND_MAX) * 6.283185f;
								
								listAsteroids.push_back({
									true, // alive
									a.pos, // pos
									{ (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
									{ 50.0f * sinf(angle1) , 50.0f * cosf(angle1) }, // vel
									0.0f, // angle
									a.scale, // scale
									spriteAsteroid, // sprite
									decalAsteroid, // decal
								});

								listAsteroids.push_back({
									true, // alive
									a.pos, // pos
									{ (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
									{ 50.0f * sinf(angle2) , 50.0f * cosf(angle2) }, // vel
									0.0f, // angle
									a.scale, // scale
									spriteAsteroid, // sprite
									decalAsteroid, // decal
								});
							}
						}
					}
				}
			}
		}

		// remove dead asteroids		
		listAsteroids.remove_if([&](const sSpaceObject& s) { return !s.alive; });

		// remove dead bullets
		listBullets.remove_if([&](const sSpaceObject& s) { return !s.alive; });

		if(listAsteroids.empty())
		{
			nScore += 1000;
			listAsteroids.clear();
			listBullets.clear();
			
			// Put in two asteroids
			listAsteroids.push_back({
				true, // alive
				{ player.pos.x - 150.0f, player.pos.y }, // pos
				{ (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
				{ 10.0f , -64.0f }, // vel
				0.0f, // angle
				{ 1.0f, 1.0f }, // scale
				spriteAsteroid, // sprite
				decalAsteroid, // decal
			});

			listAsteroids.push_back({
				true, // alive
				{ player.pos.x + 150.0f, player.pos.y }, // pos
				{ (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
				{ -10.0f , 64.0f }, // vel
				0.0f, // angle
				{ 1.0f, 1.0f }, // scale
				spriteAsteroid, // sprite
				decalAsteroid, // decal
			});
		}
		
		// DRAW EVERYTHING
		DrawSpaceObjects(listAsteroids);
		DrawSpaceObjects(listBullets);
		player.Draw(*this);
		explosions->Draw(fElapsedTime);
		explosions2->Draw(fElapsedTime);

		DrawStringDecal({ 43.0f, 23.0f }, std::to_string(nScore), olc::BLACK, { 3.0f, 3.0f });
		DrawStringDecal({ 40.0f, 20.0f }, std::to_string(nScore), olc::WHITE, { 3.0f, 3.0f });
		return true;
	}

    void ResetGame()
    {
        nScore = 0;
        
        listAsteroids.clear();
        listBullets.clear();

        player = {
            true, // alive
            { ((float)ScreenWidth() / 2), ((float)ScreenHeight() / 2) }, // pos
            { (float)spriteShip->width / 2, (float)spriteShip->height  / 2 }, // origin
            { 0.0f, 0.0f }, // vel
            0.0f, // angle
            { 0.5f, 0.5f }, // scale
            spriteShip, // sprite
            decalShip, // decal
        };

        // Put in two asteroids
        listAsteroids.push_back({
            true, // alive
            { player.pos.x - 150.0f, player.pos.y }, // pos
            { (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
            { 10.0f , -64.0f }, // vel
            0.0f, // angle
            { 1.0f, 1.0f }, // scale
            spriteAsteroid, // sprite
            decalAsteroid, // decal
        });

        listAsteroids.push_back({
            true, // alive
            { player.pos.x + 150.0f, player.pos.y }, // pos
            { (float)spriteAsteroid->width / 2, (float)spriteAsteroid->height  / 2 }, // origin
            { -10.0f , 64.0f }, // vel
            0.0f, // angle
            { 1.0f, 1.0f }, // scale
            spriteAsteroid, // sprite
            decalAsteroid, // decal
        });
    }

    void DrawSpaceObjects(std::list<sSpaceObject> &objects)
    {
        for(auto &o : objects)
        {
            if(o.alive)
                o.Draw(*this);
        }
    }

private:
	olc::Sprite *spriteBackground;
	
	olc::Sprite *spriteAsteroid;
	olc::Sprite *spriteBullet;
	olc::Sprite *spriteShip;
	
	olc::Decal *decalAsteroid;
	olc::Decal *decalBullet;
	olc::Decal *decalShip;
    
	int nScore;
	sSpaceObject player;
	std::list<sSpaceObject> listBullets;
	std::list<sSpaceObject> listAsteroids;
	olc::Particles *explosions;
	olc::Particles *explosions2;
};

int main()
{
	PGE_Asteroids app;
	if(app.Construct(1280, 720, 1, 1))
		app.Start();

	return 0;
}
