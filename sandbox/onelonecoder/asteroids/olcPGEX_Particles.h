/*
	olcPGEX_Particles.h

	+-------------------------------------------------------------+
	|         OneLoneCoder Pixel Game Engine Extension            |
	|                Particles - v0.0.1            	              |
	+-------------------------------------------------------------+

	What is this?
	~~~~~~~~~~~~~
	This is an extension to the olcPixelGameEngine, which provides
	the ability to easily animate particles.

	License (OLC-3)
	~~~~~~~~~~~~~~~

	Copyright 2018 - 2019 OneLoneCoder.com

	Redistribution and use in source and binary forms, with or without
	modification, are permitted provided that the following conditions
	are met:

	1. Redistributions or derivations of source code must retain the above
	copyright notice, this list of conditions and the following disclaimer.

	2. Redistributions or derivative works in binary form must reproduce
	the above copyright notice. This list of conditions and the following
	disclaimer must be reproduced in the documentation and/or other
	materials provided with the distribution.

	3. Neither the name of the copyright holder nor the names of its
	contributors may be used to endorse or promote products derived
	from this software without specific prior written permission.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
	"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
	LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
	A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
	HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
	SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
	DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
	OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Links
	~~~~~
	Homepage:	https://www.moros1138.com

	Author
	~~~~~~
	Moros Smith aka Moros1138

	Contributors
	~~~~~~
	Matt Hayward aka SaladinAkara
*/

#ifndef OLC_PGEX_PARTICLES
#define OLC_PGEX_PARTICLES

namespace olc
{
	
	class Particles : public olc::PGEX
	{
	public:
		Particles();
		Particles(std::string sImageFile);
		Particles(std::string sImageFile, std::function<void(olc::Particles &p)> create, std::function<void(olc::Particles &p, float fElapsedTime)> move, olc::vf2d scale = {1.0f, 1.0f});
		~Particles();
	public:
		// spawns particles from the provided position
		void Spawn(olc::vf2d pos);
		// moves and draws particles
		void Draw(float fElapsedTime);
		// returns current spawn position
		olc::vf2d GetPosition();
		// set current spawn position
		void SetPosition(olc::vf2d pos);
		// returns current center point
		olc::vf2d GetCenter();
		// sets current center point
		void SetCenter(olc::vf2d center);
		// gets current sprite scale
		olc::vf2d GetScale();
		// sets current sprite scale
		void SetScale(olc::vf2d scale);
		// sets the create particle animation lambda function
		void SetCreateFunction(std::function<void(olc::Particles &p)> create);
		// sets the move particle animation lambda function
		void SetMoveFunction(std::function<void(olc::Particles &p, float fElapsedTime)> move);

		// particle structure
		struct sParticle {
			olc::vf2d pos = { 0.0f, 0.0f };
			olc::vf2d vel = { 0.0f, 0.0f };
			olc::vf2d scale = { 1.0f, 1.0f };
			float angle = 0.0f;
			olc::Pixel tint = olc::WHITE;
			bool remove = false;
		};
		
		// list of particles, used in lambda create/move functions as well as Draw
		std::list<sParticle> lParticles;
		
	private: // private functions
		// load the sprite to use as the particle
		void LoadSprite(std::string sImageFile);

	private:
		// particle sprite
		olc::Sprite* sprite;
		// particle decal
		olc::Decal* decal;
		// position of the current spawn
		olc::vf2d pos;
		// center used as origin for rotated sprites
		olc::vf2d center;
		// overall scale of the particle sprite
		olc::vf2d scale;
		// called by Spawn to spawn particles based on the lambda function
		std::function<void(olc::Particles &p)> createFunc;
		// called by Draw to move particles based on the lambda function
		std::function<void(olc::Particles &p, float fElapsedTime)> moveFunc;
	};

}

#ifdef OLC_PGEX_PARTICLES_APP
#undef OLC_PGEX_PARTICLES_APP

namespace olc
{
	namespace ParticleAnimations
	{
		auto LinearCreate = [](olc::Particles &p)
		{
			int particles = 25;
			for(int i = 0; i < particles; i++)
			{
				float speed = ((float)rand() / (float)RAND_MAX) * 200.0f;
				float fTheta = 2.0f * 3.14159f / particles;

				p.lParticles.push_back({
					p.GetPosition(), // pos
					{ 180.0f * cosf(fTheta * i), 180.0f * sinf(fTheta * i)}, // vel
					{ 0.1f, 0.1f }  // scale
				});
			}
		};
		
		auto LinearMove = [](olc::Particles &p, float fElapsedTime)
		{
			for(auto &particle : p.lParticles)
			{
				particle.pos += particle.vel * fElapsedTime;
				
				particle.scale.x += particle.scale.x * 2.0f * fElapsedTime;
				particle.scale.y += particle.scale.y * 2.0f * fElapsedTime;
				
				int temp = (255 - ((particle.scale.x / 0.9f) * 254));

				if(temp < 0)
					particle.tint.a = 0;
				else
					particle.tint.a = (uint8_t)temp;
				
				particle.tint.r = particle.tint.a;
				particle.tint.g = particle.tint.a;
				particle.tint.b = particle.tint.a;

				if(particle.tint.a == 0)
					particle.remove = true;
			}

			p.lParticles.remove_if([&](olc::Particles::sParticle s) { return s.remove; });
		};


		auto StarBurstCreate = [](olc::Particles &p)
		{
			int particles = 25;
			for(int i = 0; i < particles; i++)
			{
				float angle = ((float)rand() / (float)RAND_MAX) * 2.0f * 3.14159f;
				float speed = ((float)rand() / (float)RAND_MAX) * 200.0f;

				p.lParticles.push_back({
					p.GetPosition(), // pos
					{ cosf(angle)*speed, sinf(angle)*speed }, // vel
					{ 0.1f, 0.1f }  // scale
				});
			}
		};
		
		auto StarBurstMove = [](olc::Particles &p, float fElapsedTime)
		{
			for(auto &particle : p.lParticles)
			{
				particle.pos += particle.vel * fElapsedTime;
				
				particle.scale.x += particle.scale.x * 2.0f * fElapsedTime;
				particle.scale.y += particle.scale.y * 2.0f * fElapsedTime;
				
				if(particle.vel.x > 0.0f)
					particle.angle += 1.0f * fElapsedTime;
				else
					particle.angle -= 1.0f * fElapsedTime;
				
				int temp = (255 - ((particle.scale.x / 0.9f) * 254));

				if(temp < 0)
					particle.tint.a = 0;
				else
					particle.tint.a = (uint8_t)temp;
				
				particle.tint.r = particle.tint.a;
				particle.tint.g = particle.tint.a;
				particle.tint.b = particle.tint.a;

				if(particle.tint.a == 0)
					particle.remove = true;
			}

			p.lParticles.remove_if([&](olc::Particles::sParticle s) { return s.remove; });
		};
	}

	Particles::Particles()
	{
		// default sprite
		sprite = new olc::Sprite(20, 20);
		
		// draw a grey block
		pge->SetDrawTarget(sprite);
		pge->Clear(olc::GREY);
		pge->SetDrawTarget(nullptr);

		// create the decal
		decal = new olc::Decal(sprite);

		// update scale
		scale = { 1.0f, 1.0f };

		// update center
		center = {(float)sprite->width / 2, (float)sprite->height / 2};

		// set the create/move functions
		createFunc = olc::ParticleAnimations::LinearCreate;
		moveFunc = olc::ParticleAnimations::LinearMove;
	}
	
	Particles::Particles(std::string sImageFile)
	{
		LoadSprite(sImageFile);
		
		// update scale
		scale = {1.0f, 1.0f};

		// update center
		center = {(float)sprite->width / 2, (float)sprite->height / 2};

		// set the create/move functions
		createFunc = olc::ParticleAnimations::LinearCreate;
		moveFunc = olc::ParticleAnimations::LinearMove;
	}

	Particles::Particles(std::string sImageFile, std::function<void(olc::Particles &p)> create, std::function<void(olc::Particles &p, float fElapsedTime)> move, olc::vf2d scale)
	{
		LoadSprite(sImageFile);
		
		// update scale
		this->scale = scale;
		
		// update center
		center = {(float)sprite->width / 2, (float)sprite->height / 2};

		// set the create/move functions
		createFunc = create;
		moveFunc = move;
	}

	// destructor
	Particles::~Particles()
	{
		delete decal;
		delete sprite;
	}

	// load the sprite to use as the particle
	void Particles::LoadSprite(std::string sImageFile)
	{
		sprite = new olc::Sprite(sImageFile);
		decal = new olc::Decal(sprite);
	}	
	
	// spawn at a new position
	void Particles::Spawn(olc::vf2d pos)
	{
		// update position
		this->pos = pos;
		
		// create all the new particles
		createFunc(*this);
	}

	// draws the particles, if any
	void Particles::Draw(float fElapsedTime)
	{
		// no particles?
		if(lParticles.size() == 0)
			return;

		// move all the particles
		moveFunc(*this, fElapsedTime);
		
		// draw all the particles, to scale
		for(auto &p : lParticles)
		{
			pge->DrawRotatedDecal(p.pos, decal, p.angle, center, p.scale * scale, p.tint);
		}
	}
	
	// pos getter
	olc::vf2d Particles::GetPosition()
	{
		return pos;
	}
	
	// pos setter
	void Particles::SetPosition(olc::vf2d pos)
	{
		this->pos = pos;
	}
	
	// center getter
	olc::vf2d Particles::GetCenter()
	{
		return center;
	}

	// center setter
	void Particles::SetCenter(olc::vf2d center)
	{
		this->center = center;
	}
	
	// scale getter
	olc::vf2d Particles::GetScale()
	{
		return scale;
	}
	
	// scale setter
	void Particles::SetScale(olc::vf2d scale)
	{
		this->scale = scale;
	}
	
	// createFunc setter
	void Particles::SetCreateFunction(std::function<void(olc::Particles &p)> create)
	{
		createFunc = create;
	}
	
	// moveFunc setter
	void Particles::SetMoveFunction(std::function<void(olc::Particles &p, float fElapsedTime)> move)
	{
		moveFunc = move;
	}
}

#endif
#endif