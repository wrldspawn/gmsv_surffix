// vim: tabstop=2 shiftwidth=2 noexpandtab

#include <GarrysMod/FactoryLoader.hpp>
#include <GarrysMod/Lua/Interface.h>
#include <GarrysMod/Symbol.hpp>
#include <GarrysMod/InterfacePointers.hpp>

#include <dbg.h>

#include <iconvar.h>
#include <movehelper_server.h>
#include <in_buttons.h>
#include "overrides/gamemovement.h"

#include <detouring/hook.hpp>
#include <scanning/symbolfinder.hpp>

// TryPlayerMove: "PM  Got a NaN velocity %s\n" -> xref to CheckVelocity -> xref to function thats second to last call,
//                the last call will be a virtual call, take the offset and add it to the base address of CGameMovement
//                vtable
//
// CGameMovement vtable: (not needed on 64bit) instruction to call to offset pointing to vtable, offset var is bytes to
//                       skip
//
// surfaceFriction: Look for `((1.0 - *(var + offset)) * var) + 1.0` in TryPlayerMove
//
// ShouldHitEntity: CTraceFilterSimple vtable, will be the only unique function (usually the first) compared to the
//                  vtables of the other filter classes
//
// GetGroundEntity: "Player.Swim" -> xref to function that has `&= ~2` (FullWalkMove) -> find function call on this[1]
//                  thats followed by `*(this[2] + offset) = 0;`
//
// MoveHelperServer: xref CMoveHelperServer vtable to function that references it and IMoveHelper vtable -> xref up
//
// g_pEntityList: "ents.GetAll" xref, offset is bytes to skip to address
#if SYSTEM_IS_WINDOWS
#if ARCHITECTURE_IS_X86_64
int off_surfaceFriction = 11032;
int off_CGameMovement = 0;
Symbol sym_CGameMovement = Symbol::FromSignature("\xFF");
Symbol sym_TryPlayerMove = Symbol::FromSignature("\x4C\x8B\xDC\x49\x89\x5B*\x49\x89\x73*\x49\x89\x7B*\x55\x41\x54");
Symbol sym_ShouldHitEntity = Symbol::FromSignature("\x48\x89\x5C\x24*\x48\x89\x6C\x24*\x48\x89\x74\x24*\x48"
                                                   "\x89\x7C\x24*\x41\x56\x48\x83\xEC*\x48\x8B\xE9\x41\x8B\xF8");
Symbol sym_GetGroundEntity = Symbol::FromSignature("\x8B\x91\xA8\x02\x00\x00");
Symbol sym_MoveHelperServer = Symbol::FromSignature("\x40\x53\x48\x83\xEC*\x65\x48\x8B\x04\x25****\x8B\x0D****\xBA*"
                                                    "**********\x39\x05");
int off_g_pEntityList = 5;
Symbol sym_g_pEntityList =
    Symbol::FromSignature("\x33\xD2\x48\x8D\x0D****\xBF****\xE8****\x48\x8B\xD8\x48\x85\xC0\x74");
#else
int off_surfaceFriction = 10336;
int off_CGameMovement = 5;
Symbol sym_CGameMovement = Symbol::FromSignature("\x56\x8B\xF1\xC7\x06****\x74");
Symbol sym_TryPlayerMove = Symbol::FromSignature("\x55\x8B\xEC\x81\xEC****\x56\x57\x33\xC0");
Symbol sym_ShouldHitEntity = Symbol::FromSignature("\x55\x8B\xEC\x51\x89\x4D*\x8B\x0D****\x53\x56");
Symbol sym_GetGroundEntity = Symbol::FromSignature("\x8B\x91\xFC\x01\x00\x00");
Symbol sym_MoveHelperServer = Symbol::FromSignature("\xB9****\xC7\x05********\xC7"
                                                    "\x05********\xE8****\xC7\x05");
int off_g_pEntityList = 3;
Symbol sym_g_pEntityList = Symbol::FromSignature("\x6A*\xB9****\xBF");
#endif
#elif SYSTEM_IS_LINUX
#if ARCHITECTURE_IS_X86_64
int off_surfaceFriction = 0; // TODO
int off_CGameMovement = 0;
Symbol sym_CGameMovement = Symbol::FromSignature("\xFF");
Symbol sym_TryPlayerMove = Symbol::FromSignature("\x4C\x8B\xDC\x49\x89\x5B*\x49\x89\x73*\x49\x89"
                                                 "\x7B*\x55\x41\x54"); // TODO
Symbol sym_ShouldHitEntity =
    Symbol::FromSignature("\x48\x89\x5C\x24\x00\x48\x89\x6C\x24\x00\x48\x89\x74\x24\x00\x48\x89\x7C\x24\x00\x41\x56\x48"
                          "\x83\xEC\x00\x48\x8B\xE9\x41\x8B\xF8");              // TODO
Symbol sym_GetGroundEntity = Symbol::FromSignature("\x8B\x91\xA8\x02\x00\x00"); // TODO
Symbol sym_MoveHelperServer = Symbol::FromSignature("\x48\x89\x5C\x24*\x57\x48\x83\xEC*\x48\x8D\x05****\x48\x8B\xF9***"
                                                    "\x8B\xDA\x48\x83\xC1*\x48\xC7\x05"); // TODO
Symbol sym_g_pEntityList = Symbol::FromSignature("\x48\x8D\x0D\x51\x62\xC6\x00");         // TODO
#else
int off_surfaceFriction = 0; // TODO
int off_CGameMovement = 0;
Symbol sym_CGameMovement = Symbol::FromSignature("\xFF");
Symbol sym_TryPlayerMove = Symbol::FromSignature("\x4C\x8B\xDC\x49\x89\x5B*\x49\x89\x73*\x49\x89"
                                                 "\x7B*\x55\x41\x54"); // TODO
Symbol sym_ShouldHitEntity =
    Symbol::FromSignature("\x48\x89\x5C\x24\x00\x48\x89\x6C\x24\x00\x48\x89\x74\x24\x00\x48\x89\x7C\x24\x00\x41\x56\x48"
                          "\x83\xEC\x00\x48\x8B\xE9\x41\x8B\xF8");              // TODO
Symbol sym_GetGroundEntity = Symbol::FromSignature("\x8B\x91\xA8\x02\x00\x00"); // TODO
Symbol sym_MoveHelperServer = Symbol::FromSignature("\x48\x89\x5C\x24*\x57\x48\x83\xEC*\x48\x8D\x05****\x48\x8B\xF9***"
                                                    "\x8B\xDA\x48\x83\xC1*\x48\xC7\x05"); // TODO
Symbol sym_g_pEntityList = Symbol::FromSignature("\x48\x8D\x0D\x51\x62\xC6\x00");         // TODO
#endif
#endif

typedef int(__thiscall *TryPlayerMove_t)(CGameMovement *, Vector *, trace_t *);
typedef bool(__thiscall *ShouldHitEntity_t)(CTraceFilterSimple *, IHandleEntity *, int);
typedef CBaseEntity *(__thiscall *GetGroundEntity_t)(CBaseEntity *);
typedef IMoveHelperServer *(*MoveHelperServer_t)();

CGlobalVars *gpGlobals = nullptr;
CBaseEntityList *g_pEntityList = nullptr;
CGameMovement *vt_CGameMovement = nullptr;

Detouring::Hook detour_TryPlayerMove;
TryPlayerMove_t func_TryPlayerMove = nullptr;
ShouldHitEntity_t func_ShouldHitEntity = nullptr;
GetGroundEntity_t func_GetGroundEntity = nullptr;
MoveHelperServer_t func_MoveHelperServer = nullptr;

CTraceFilterSimple::CTraceFilterSimple(const IHandleEntity *passedict, int collisionGroup,
                                       ShouldHitFunc_t pExtraShouldHitFunc) {
	m_pPassEnt = passedict;
	m_collisionGroup = collisionGroup;
	m_pExtraShouldHitCheckFunction = pExtraShouldHitFunc;
}

bool CTraceFilterSimple::ShouldHitEntity(IHandleEntity *pHandleEntity, int contentsMask) {
	return func_ShouldHitEntity(this, pHandleEntity, contentsMask);
}

inline void surffix_TraceRay(const Ray_t &ray, unsigned int mask, const IHandleEntity *ignore, int collisionGroup,
                             trace_t *ptr, ShouldHitFunc_t pExtraShouldHitCheckFn = NULL) {
	CTraceFilterSimple traceFilter(ignore, collisionGroup, pExtraShouldHitCheckFn);

	enginetrace->TraceRay(ray, mask, &traceFilter, ptr);
}

#define MAX_CLIP_PLANES 5
#define NON_JUMP_VELOCITY 140.0f

static ConVar *sv_bounce = nullptr;

static ConVar sv_slope_fix("sv_slope_fix", "1", FCVAR_ARCHIVE, "[gmsv_surffix] Fix slopes");
static ConVar sv_ramp_fix("sv_ramp_fix", "1", FCVAR_ARCHIVE, "[gmsv_surffix] Fix ramps");
static ConVar sv_ramp_bumpcount("sv_ramp_bumpcount", "8", FCVAR_ARCHIVE,
                                "[gmsv_surffix] Helps with fixing surf/ramp bugs", true, 4, true, 16);
static ConVar sv_ramp_initial_retrace_length("sv_ramp_initial_retrace_length", "0.2", FCVAR_ARCHIVE,
                                             "[gmsv_surffix] Amount of units used in offset for retraces", true, 0.2f,
                                             true, 5.f);

static ConVar sv_rngfix_enable("sv_rngfix_enable", "0", FCVAR_ARCHIVE, "[gmsv_surffix] Enable rngfix");

// copy pasting this because one less thing to find and whatever i found was wrong and broke everything :)
static int OrigClipVelocity(CGameMovement *self, Vector in, Vector &normal, Vector &out, float overbounce) {
	int blocked = 0x00; // Assume unblocked.
	if (normal[2] > 0)  // If the plane that is blocking us has a positive z component, then assume it's a floor.
		blocked |= 0x01;  // Floor = 1
	if (CloseEnough(normal[2], 0.0f, FLT_EPSILON)) // If the plane has no Z, it is vertical (wall/step)
		blocked |= 0x02;                             // Wall = 2

	// Determine how far along plane to slide based on incoming direction.
	float backoff = DotProduct(in, normal) * overbounce;

	for (int i = 0; i < 3; i++) {
		float change = normal[i] * backoff;
		out[i] = in[i] - change;
	}

	// iterate once to make sure we aren't still moving through the plane
	float adjust = DotProduct(out, normal);
	if (adjust < 0.0f) {
		out -= (normal * adjust);
	}

	// Return blocking flags.
	return blocked;
}

static int ClipVelocity(CGameMovement *self, Vector in, Vector &normal, Vector &out, float overbounce) {
	const int blocked = OrigClipVelocity(self, in, normal, out, overbounce);

	if (sv_rngfix_enable.GetBool())
		return blocked;

	// Check if the jump button is held to predict if the player wants to jump up an incline. Not checking for jumping
	// could allow players that hit the slope almost perpendicularly and still surf up the slope because they would
	// retain their horizontal speed
	if (sv_slope_fix.GetBool() && (self->mv->m_nButtons & IN_JUMP)) {
		bool canJump = normal[2] >= 0.7f && out.z <= NON_JUMP_VELOCITY;

		// if (m_pPlayer->m_CurrentSlideTrigger)
		// canJump &= m_pPlayer->m_CurrentSlideTrigger->m_bAllowingJump;

		// If the player do not gain horizontal speed while going up an incline, then act as if the surface is flat
		if (canJump && (normal.x * in.x + normal.y * in.y < 0.0f) && out.Length2DSqr() <= in.Length2DSqr()) {
			out.x = in.x;
			out.y = in.y;
			out.z = 0.0f;
		}
	}

	// Return blocking flags.
	return blocked;
}

static bool IsValidMovementTrace(CGameMovement *self, trace_t &tr) {
	trace_t stuck;

	// Apparently we can be stuck with pm.allsolid without having valid plane info ok..
	if (tr.allsolid || tr.startsolid) {
		return false;
	}

	// Maybe we don't need this one
	if (CloseEnough(tr.fraction, 0.0f, FLT_EPSILON)) {
		return false;
	}

	if (CloseEnough(tr.fraction, 0.0f, FLT_EPSILON) &&
	    CloseEnough(tr.plane.normal, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON)) {
		return false;
	}

	// Is the plane deformed or some stupid shit?
	if (fabs(tr.plane.normal.x) > 1.0f || fabs(tr.plane.normal.y) > 1.0f || fabs(tr.plane.normal.z) > 1.0f) {
		return false;
	}

	self->TracePlayerBBox(tr.endpos, tr.endpos, self->PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT, stuck);
	if (stuck.startsolid || !CloseEnough(stuck.fraction, 1.0f, FLT_EPSILON)) {
		return false;
	}

	return true;
}

static int hook_TryPlayerMove(CGameMovement *self, Vector *pFirstDest, trace_t *pFirstTrace) {
	// 32bit weirdness
	if (self == nullptr && vt_CGameMovement != nullptr) {
		self = vt_CGameMovement;
	}

	auto mv = self->mv;
	auto player = self->player;
	float m_surfaceFriction = *((float *)player + off_surfaceFriction);

	int bumpcount, numbumps;
	Vector dir;
	float d;
	int numplanes;
	Vector planes[MAX_CLIP_PLANES];
	Vector primal_velocity, original_velocity;
	Vector new_velocity;
	Vector fixed_origin;
	Vector valid_plane;
	int i, j, h;
	trace_t pm;
	Vector end;
	float time_left, allFraction;
	int blocked;
	bool stuck_on_ramp;
	bool has_valid_plane;
	numbumps = sv_ramp_bumpcount.GetInt();

	blocked = 0;   // Assume not blocked
	numplanes = 0; // and not sliding along any planes

	stuck_on_ramp = false;   // lets assume client isn't stuck already
	has_valid_plane = false; // no plane info gathered yet

	VectorCopy(mv->m_vecVelocity, original_velocity); // Store original velocity
	VectorCopy(mv->m_vecVelocity, primal_velocity);
	VectorCopy(mv->GetAbsOrigin(), fixed_origin);

	allFraction = 0;
	time_left = gpGlobals->frametime; // Total time for this movement operation.

	new_velocity.Init();
	valid_plane.Init();

	Vector vecWallNormal;

	for (bumpcount = 0; bumpcount < numbumps; bumpcount++) {
		if (mv->m_vecVelocity.Length() == 0.0)
			break;

		if (stuck_on_ramp && sv_ramp_fix.GetBool()) {
			if (!has_valid_plane) {
				if (!CloseEnough(pm.plane.normal, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON) && valid_plane != pm.plane.normal) {
					valid_plane = pm.plane.normal;
					has_valid_plane = true;
				} else {
					for (i = numplanes; i-- > 0;) {
						if (!CloseEnough(planes[i], Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON) && fabs(planes[i].x) <= 1.0f &&
						    fabs(planes[i].y) <= 1.0f && fabs(planes[i].z) <= 1.0f && valid_plane != planes[i]) {
							valid_plane = planes[i];
							has_valid_plane = true;
							break;
						}
					}
				}
			}

			if (has_valid_plane) {
				if (valid_plane.z >= 0.7f && valid_plane.z <= 1.0f) {
					ClipVelocity(self, mv->m_vecVelocity, valid_plane, mv->m_vecVelocity, 1);
					VectorCopy(mv->m_vecVelocity, original_velocity);
				} else {
					ClipVelocity(self, mv->m_vecVelocity, valid_plane, mv->m_vecVelocity,
					             1.0f + sv_bounce->GetFloat() * (1.0f - m_surfaceFriction));
					VectorCopy(mv->m_vecVelocity, original_velocity);
				}
			} else { // We were actually going to be stuck, lets try and find a valid plane..
				// this way we know fixed_origin isn't going to be stuck
				float offsets[] = {(bumpcount * 2) * -sv_ramp_initial_retrace_length.GetFloat(), 0.0f,
				                   (bumpcount * 2) * sv_ramp_initial_retrace_length.GetFloat()};

				int valid_planes = 0;
				valid_plane.Init();

				// we have 0 plane info, so lets increase our bbox and search in all 27 directions to get a valid plane!
				for (i = 0; i < 3; i++) {
					for (j = 0; j < 3; j++) {
						for (h = 0; h < 3; h++) {
							Vector offset = {offsets[i], offsets[j], offsets[h]};

							Vector offset_mins = offset / 2.0f;
							Vector offset_maxs = offset / 2.0f;

							if (offset.x > 0.0f)
								offset_mins.x /= 2.0f;
							if (offset.y > 0.0f)
								offset_mins.y /= 2.0f;
							if (offset.z > 0.0f)
								offset_mins.z /= 2.0f;

							if (offset.x < 0.0f)
								offset_maxs.x /= 2.0f;
							if (offset.y < 0.0f)
								offset_maxs.y /= 2.0f;
							if (offset.z < 0.0f)
								offset_maxs.z /= 2.0f;

							Ray_t ray;
							ray.Init(fixed_origin + offset, end - offset, self->GetPlayerMins() - offset_mins,
							         self->GetPlayerMaxs() + offset_maxs);
							surffix_TraceRay(ray, self->PlayerSolidMask(), mv->m_nPlayerHandle.Get(), COLLISION_GROUP_PLAYER_MOVEMENT,
							                 &pm);

							// Only use non deformed planes and planes with values where the start point is not from a solid
							if (fabs(pm.plane.normal.x) <= 1.0f && fabs(pm.plane.normal.y) <= 1.0f &&
							    fabs(pm.plane.normal.z) <= 1.0f && pm.fraction > 0.0f && pm.fraction < 1.0f && !pm.startsolid) {
								valid_planes++;
								valid_plane += pm.plane.normal;
							}
						}
					}
				}

				if (valid_planes && !CloseEnough(valid_plane, Vector(0.0f, 0.0f, 0.0f), FLT_EPSILON)) {
					has_valid_plane = true;
					valid_plane.NormalizeInPlace();
					continue;
				}
			}

			if (has_valid_plane) {
				VectorMA(fixed_origin, sv_ramp_initial_retrace_length.GetFloat(), valid_plane, fixed_origin);
			} else {
				stuck_on_ramp = false;
				continue;
			}
		}

		// Assume we can move all the way from the current origin to the end point.
		VectorMA(fixed_origin, time_left, mv->m_vecVelocity, end);

		// See if we can make it from origin to end point.
		// If their velocity Z is 0, then we can avoid an extra trace here during WalkMove.
		if (pFirstDest && end == *pFirstDest) {
			pm = *pFirstTrace;
		} else {
			if (stuck_on_ramp && has_valid_plane && sv_ramp_fix.GetBool()) {
				self->TracePlayerBBox(fixed_origin, end, self->PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT, pm);
				pm.plane.normal = valid_plane;
			} else {
				self->TracePlayerBBox(mv->GetAbsOrigin(), end, self->PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT, pm);

				if (sv_rngfix_enable.GetBool() && sv_slope_fix.GetBool() && player->GetMoveType() == MOVETYPE_WALK &&
				    func_GetGroundEntity(player) == nullptr && player->GetWaterLevel() < WL_Waist) {
					bool bValidHit = !pm.allsolid && pm.fraction < 1.0f;

					bool bCouldStandHere = pm.plane.normal.z >= 0.7f && mv->m_vecVelocity.z <= NON_JUMP_VELOCITY;
					// && m_pPlayer->m_CurrentSlideTrigger == nullptr;

					bool bMovingIntoPlane2D =
					    (pm.plane.normal.x * mv->m_vecVelocity.x) + (pm.plane.normal.y * mv->m_vecVelocity.y) < 0.0f;

					// Don't perform this fix on additional collisions this tick which have trace fraction == 0.0.
					// This situation occurs when wedged between a standable slope and a ceiling.
					// The player would be locked in place with full velocity (but no movement) without this check.
					bool bWedged = /*m_pPlayer->GetInteraction(0).tick == gpGlobals->tickcount &&*/ pm.fraction == 0.0f;

					if (bValidHit && bCouldStandHere && bMovingIntoPlane2D && !bWedged) {
						Vector vecNewVelocity;
						ClipVelocity(self, mv->m_vecVelocity, pm.plane.normal, vecNewVelocity, 1.0f);

						// Make sure allowing this collision would not actually be beneficial (2D speed gain)
						if (vecNewVelocity.Length2DSqr() <= mv->m_vecVelocity.Length2DSqr()) {
							// A fraction of 1.0 implies no collision, which means ClipVelocity will not be called.
							// It also suggests movement for this tick is complete, so TryPlayerMove won't perform
							// additional movement traces and the tick will essentially end early. We want this to
							// happen because we need landing/jumping logic to be applied before movement continues.
							// Ending the tick early is a safe and easy way to do this.

							pm.fraction = 1.0f;
						}
					}
				}
			}
		}

		if (bumpcount && sv_ramp_fix.GetBool() && func_GetGroundEntity(player) == nullptr &&
		    !IsValidMovementTrace(self, pm)) {
			has_valid_plane = false;
			stuck_on_ramp = true;
			continue;
		}

		// If we started in a solid object, or we were in solid space
		// the whole way, zero out our velocity and return that we
		// are blocked by floor and wall.
		if (pm.allsolid && !sv_ramp_fix.GetBool()) {
			//  entity is trapped in another solid
			VectorCopy(vec3_origin, mv->m_vecVelocity);

			return 4;
		}

		// If we moved some portion of the total distance, then
		//  copy the end position into the pmove.origin and
		//  zero the plane counter.
		if (pm.fraction > 0.0f) {
			if ((!bumpcount || func_GetGroundEntity(player) != nullptr || !sv_ramp_fix.GetBool()) && numbumps > 0 &&
			    pm.fraction == 1.0f) {
				// There's a precision issue with terrain tracing that can cause a swept box to successfully trace
				// when the end position is stuck in the triangle.  Re-run the test with an unswept box to catch that
				// case until the bug is fixed.
				// If we detect getting stuck, don't allow the movement
				trace_t stuck;
				self->TracePlayerBBox(pm.endpos, pm.endpos, self->PlayerSolidMask(), COLLISION_GROUP_PLAYER_MOVEMENT, stuck);

				if ((stuck.startsolid || stuck.fraction != 1.0f) && !bumpcount && sv_ramp_fix.GetBool()) {
					has_valid_plane = false;
					stuck_on_ramp = true;
					continue;
				} else if (stuck.startsolid || stuck.fraction != 1.0f) {
					VectorCopy(vec3_origin, mv->m_vecVelocity);
					break;
				}
			}

			if (sv_ramp_fix.GetBool()) {
				has_valid_plane = false;
				stuck_on_ramp = false;
			}

			// actually covered some distance
			VectorCopy(mv->m_vecVelocity, original_velocity);
			mv->SetAbsOrigin(pm.endpos);
			VectorCopy(mv->GetAbsOrigin(), fixed_origin);
			allFraction += pm.fraction;
			numplanes = 0;
		}

		// If we covered the entire distance, we are done
		//  and can return.
		if (CloseEnough(pm.fraction, 1.0f, FLT_EPSILON)) {
			break; // moved the entire distance
		}

		// Save entity that blocked us (since fraction was < 1.0) for contact
		// Add it if it's not already in the list!!!
		func_MoveHelperServer()->AddToTouched(pm, mv->m_vecVelocity);

		// If the plane we hit has a high z component in the normal, then it's probably a floor
		if (pm.plane.normal[2] > 0.7) {
			blocked |= 1; // floor
		}

		// If the plane has a zero z component in the normal, then it's a step or wall
		if (CloseEnough(pm.plane.normal[2], 0.0f, FLT_EPSILON)) {
			blocked |= 2; // step / wall
		}

		// Reduce amount of m_flFrameTime left by total time left * fraction that we covered.
		time_left -= time_left * pm.fraction;

		// Did we run out of planes to clip against?
		if (numplanes >= MAX_CLIP_PLANES) {
			// this shouldn't really happen
			//  Stop our movement if so.
			VectorCopy(vec3_origin, mv->m_vecVelocity);

			break;
		}

		// Set up next clipping plane
		VectorCopy(pm.plane.normal, planes[numplanes]);
		numplanes++;

		// modify original_velocity so it parallels all of the clip planes
		//

		// reflect player velocity
		// Only give this a try for first impact plane because you can get yourself stuck in an acute corner by
		// jumping in place
		//  and pressing forward and nobody was really using this bounce/reflection feature anyway...
		if (numplanes == 1 && player->GetMoveType() == MOVETYPE_WALK && func_GetGroundEntity(player) == nullptr) {
			// Is this a floor/slope that the player can walk on?
			if (planes[0][2] > 0.7) {
				ClipVelocity(self, original_velocity, planes[0], new_velocity, 1);
				VectorCopy(new_velocity, original_velocity);
			} else { // either the player is surfing or slammed into a wall
				ClipVelocity(self, original_velocity, planes[0], new_velocity,
				             1.0f + sv_bounce->GetFloat() * (1.0f - m_surfaceFriction));
			}

			VectorCopy(new_velocity, mv->m_vecVelocity);
			VectorCopy(new_velocity, original_velocity);
		} else {
			for (i = 0; i < numplanes; i++) {
				ClipVelocity(self, original_velocity, planes[i], mv->m_vecVelocity, 1.0);
				for (j = 0; j < numplanes; j++) {
					if (j != i) {
						// Are we now moving against this plane?
						if (mv->m_vecVelocity.Dot(planes[j]) < 0) {
							break; // not ok
						}
					}
				}

				if (j == numplanes) { // Didn't have to clip, so we're ok
					break;
				}
			}

			// Did we go all the way through plane set
			if (i != numplanes) {
				// go along this plane
				// pmove.velocity is set in clipping call, no need to set again.
			} else { // go along the crease
				if (numplanes != 2) {
					// Msg("numplanes != 2\n");
					VectorCopy(vec3_origin, mv->m_vecVelocity);
					break;
				}

				// Fun fact time: these next five lines of code fix (vertical) rampbug
				if (CloseEnough(planes[0], planes[1], FLT_EPSILON)) {
					//  Why did the above return true? Well, when surfing, you can "clip" into the
					//  ramp, due to the ramp not pushing you away enough, and when that happens,
					//  a surfer cries. So the game thinks the surfer is clipping along two of the exact
					//  same planes. So what we do here is take the surfer's original velocity,
					//  and add the along the normal of the surf ramp they're currently riding down,
					//  essentially pushing them away from the ramp.

					// Note: Technically the 20.0 here can be 2.0, but that causes "jitters" sometimes, so I found
					// 20 to be pretty safe and smooth. If it causes any unforeseen consequences, tweak it!
					VectorMA(original_velocity, 20.0f, planes[0], new_velocity);
					mv->m_vecVelocity.x = new_velocity.x;
					mv->m_vecVelocity.y = new_velocity.y;
					// Note: We don't want the player to gain any Z boost/reduce from this, gravity should be the
					// only force working in the Z direction!

					// Lastly, let's get out of here before the following lines of code make the surfer lose speed.
					break;
				}

				// Though now it's good to note: the following code is needed for when a ramp creates a "V" shape,
				// and pinches the surfer between two planes of differing normals.
				CrossProduct(planes[0], planes[1], dir);
				dir.NormalizeInPlace();
				d = dir.Dot(mv->m_vecVelocity);
				VectorScale(dir, d, mv->m_vecVelocity);
			}

			//
			// if original velocity is against the original velocity, stop dead
			// to avoid tiny oscillations in sloping corners
			//
			d = mv->m_vecVelocity.Dot(primal_velocity);
			if (d <= 0) {
				VectorCopy(vec3_origin, mv->m_vecVelocity);
				break;
			}
		}
	}

	if (CloseEnough(allFraction, 0.0f, FLT_EPSILON)) {
		VectorCopy(vec3_origin, mv->m_vecVelocity);
	}

	float fLateralStoppingAmount = primal_velocity.Length2D() - mv->m_vecVelocity.Length2D();
	if (fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED) {
		float fSlamVol = (fLateralStoppingAmount > PLAYER_MAX_SAFE_FALL_SPEED * 2.0f) ? 1.0f : 0.85f;

		// Play rough landing sound with last traced surface.
		self->PlayerRoughLandingEffects(fSlamVol);
	}

	return blocked;
}

inline char *RelativeToAbsolute(char *address, int offset) {
	uint32_t rel = *reinterpret_cast<uint32_t *>((int *)(address + offset));
	return address + rel + (4 + offset);
}

static ICvar *pCvar = nullptr;
static IEngineTrace *enginetrace = nullptr;

GMOD_MODULE_OPEN() {
	ConMsg("surffix init\n");

	SymbolFinder symfinder;
	SourceSDK::FactoryLoader engine_loader("engine");
	SourceSDK::FactoryLoader server_loader("server");

	pCvar = InterfacePointers::Cvar();
	if (pCvar == nullptr) {
		LUA->ThrowError("Failed to get ICvar");
		return 0;
	}

	sv_bounce = pCvar->FindVar("sv_bounce");
	if (sv_bounce == nullptr) {
		LUA->ThrowError("Failed to find convar sv_bounce");
		return 0;
	}

	auto _entitylist =
	    symfinder.Resolve(server_loader.GetModule(), sym_g_pEntityList.name.c_str(), sym_g_pEntityList.length);
	if (_entitylist == nullptr) {
		LUA->ThrowError("Failed to find entity list");
		return 0;
	}
	// ConMsg("entity list find addr: 0x%I64x\n", _entitylist);

	auto entitylist = RelativeToAbsolute((char *)_entitylist, off_g_pEntityList);
	if (entitylist == nullptr) {
		LUA->ThrowError("Failed to get absolute address of entity list");
		return 0;
	}
	g_pEntityList = reinterpret_cast<CGlobalEntityList *>(entitylist);
	if (g_pEntityList == nullptr) {
		LUA->ThrowError("Failed to find g_pEntityList");
		return 0;
	}
	// ConMsg("g_pEntityList: 0x%I64x\n", g_pEntityList);

	enginetrace = engine_loader.GetInterface<IEngineTrace>(INTERFACEVERSION_ENGINETRACE_SERVER);
	if (enginetrace == nullptr) {
		LUA->ThrowError("Failed to get IEngineTrace");
		return 0;
	}

	IPlayerInfoManager *playerinfomanager = nullptr;
	playerinfomanager = server_loader.GetInterface<IPlayerInfoManager>(INTERFACEVERSION_PLAYERINFOMANAGER);
	if (playerinfomanager == nullptr) {
		LUA->ThrowError("Failed to get IPlayerInfoManager");
		return 0;
	}
	gpGlobals = playerinfomanager->GetGlobalVars();

	func_ShouldHitEntity = reinterpret_cast<ShouldHitEntity_t>(
	    symfinder.Resolve(server_loader.GetModule(), sym_ShouldHitEntity.name.c_str(), sym_ShouldHitEntity.length));
	if (func_ShouldHitEntity == nullptr) {
		LUA->ThrowError("Failed to find ShouldHitEntity");
		return 0;
	}

	func_GetGroundEntity = reinterpret_cast<GetGroundEntity_t>(
	    symfinder.Resolve(server_loader.GetModule(), sym_GetGroundEntity.name.c_str(), sym_GetGroundEntity.length));
	if (func_GetGroundEntity == nullptr) {
		LUA->ThrowError("Failed to find GetGroundEntity");
		return 0;
	}

	func_MoveHelperServer = reinterpret_cast<MoveHelperServer_t>(
	    symfinder.Resolve(server_loader.GetModule(), sym_MoveHelperServer.name.c_str(), sym_MoveHelperServer.length));
	if (func_MoveHelperServer == nullptr) {
		LUA->ThrowError("Failed to find MoveHelperServer");
		return 0;
	}

	if (sym_CGameMovement.length > 1) {
		ConMsg("Attempting to find CGameMovement vtable\n");
		auto _vt = symfinder.Resolve(server_loader.GetModule(), sym_CGameMovement.name.c_str(), sym_CGameMovement.length);
		if (_vt == nullptr) {
			ConMsg("Failed to find CGameMovement vtable, things might explode!\n");
		} else {
			auto vt = RelativeToAbsolute((char *)_vt, off_CGameMovement);
			if (vt == nullptr) {
				ConMsg("Failed to get CGameMovement vtable, things might explode!\n");
			} else {
				vt_CGameMovement = reinterpret_cast<CGameMovement *>(vt);
				ConMsg("CGameMovement vtable: 0x%I64x\n", vt_CGameMovement);
			}
		}
	}

	func_TryPlayerMove = reinterpret_cast<TryPlayerMove_t>(
	    symfinder.Resolve(server_loader.GetModule(), sym_TryPlayerMove.name.c_str(), sym_TryPlayerMove.length));

	if (func_TryPlayerMove == nullptr) {
		LUA->ThrowError("Failed to find TryPlayerMove");
		return 0;
	}

	// ConMsg("Got TryPlayerMove: 0x%I64x\n", func_TryPlayerMove);

	/*bool hookCreated_TryPlayerMove = */ detour_TryPlayerMove.Create(reinterpret_cast<void *>(func_TryPlayerMove),
	                                                                  reinterpret_cast<void *>(&hook_TryPlayerMove));
	/*bool hookEnabled_TryPlayerMove = */ detour_TryPlayerMove.Enable();

	// ConMsg("TryPlayerMove hooked: %d, %d\n", hookCreated_TryPlayerMove, hookEnabled_TryPlayerMove);

	return 1;
}

GMOD_MODULE_CLOSE() {
	if (detour_TryPlayerMove.IsEnabled())
		detour_TryPlayerMove.Disable();
	detour_TryPlayerMove.Destroy();
	return 0;
}
