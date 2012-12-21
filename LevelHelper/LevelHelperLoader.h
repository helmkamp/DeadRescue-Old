//  This file is part of LevelHelper
//  http://www.levelhelper.org
//
//  Created by Bogdan Vladu
//  Copyright 2011 Bogdan Vladu. All rights reserved.
//
////////////////////////////////////////////////////////////////////////////////
//
//  This software is provided 'as-is', without any express or implied
//  warranty.  In no event will the authors be held liable for any damages
//  arising from the use of this software.
//  Permission is granted to anyone to use this software for any purpose,
//  including commercial applications, and to alter it and redistribute it
//  freely, subject to the following restrictions:
//  The origin of this software must not be misrepresented; you must not
//  claim that you wrote the original software. If you use this software
//  in a product, an acknowledgment in the product documentation would be
//  appreciated but is not required.
//  Altered source versions must be plainly marked as such, and must not be
//  misrepresented as being the original software.
//  This notice may not be removed or altered from any source distribution.
//  By "software" the author refers to this code file and not the application 
//  that was used to generate this file.
//  You do not have permission to use this code or any part of it if you don't
//  own a license to LevelHelper application.
////////////////////////////////////////////////////////////////////////////////
//
//  Version history
//  ...............
//  Version for LevelHelper 1.4.9
////////////////////////////////////////////////////////////////////////////////

#import <Foundation/Foundation.h>
#import "cocos2d.h"
#import "lhConfig.h"

#ifdef LH_USE_BOX2D
#import "Box2D.h"
#endif

#import "LHNode.h"
#import "LHLayer.h"
#import "LHBatch.h"
#import "LHSprite.h"
#import "LHBezier.h"
#import "LHJoint.h"

#import "LHAnimationNode.h"
#import "LHPathNode.h"
#import "LHParallaxNode.h"
#import "LHContactNode.h"
#import "LHTouchMgr.h"
#import "LHCustomSpriteMgr.h"
#import "LHCuttingEngineMgr.h"

#import "LHCustomClasses.h"

#if __has_feature(objc_arc) && __clang_major__ >= 3
#define LH_ARC_ENABLED 1
#endif // __has_feature(objc_arc)

enum LevelHelper_TAG 
{ 
	DEFAULT_TAG 	= 0,
	COLLIDABLE 			= 1,
	NUMBER_OF_TAGS 	= 2
};

CGSize  LHSizeFromString(NSString* val);
CGRect  LHRectFromString(NSString* val);
CGPoint LHPointFromString(NSString* val);
CGPoint LHPointFromValue(NSValue* val);
NSValue* LHValueWithCGPoint(CGPoint pt);

@interface LevelHelperLoader : NSObject {
	
	NSArray* lhNodes;	//array of NSDictionary //includes LHSprite, LHBezier, LHBatch, LHLayer
    NSArray* lhJoints;	//array of NSDictionary
    NSArray* lhParallax;//array of NSDictionary 
    
    LHLayer* mainLHLayer;
    NSMutableDictionary* jointsInLevel;     //key - uniqueJointName     value - LHJoint*
    NSMutableDictionary* parallaxesInLevel; //key - uniqueParallaxName  value - LHParallaxNode*
    

	NSDictionary* wb; //world boundaries Info
    NSMutableDictionary* physicBoundariesInLevel; //keys//LHPhysicBoundarieTop
                                                        //LHPhysicBoundarieLeft
                                                        //LHPhysicBoundarieBottom
                                                        //LHPhysicBoundarieRight 
                                                    //value - LHSprite*    
            
    CGPoint safeFrame;
    CGRect  gameWorldRect;
    CGPoint gravity;
	    
    id  loadingProgressId;
    SEL loadingProgressSel;
    
    bool paused;
	CCLayer* cocosLayer; //weak ptr
#ifdef LH_USE_BOX2D
    b2World* box2dWorld; //weak ptr

    LHContactNode* contactNode;
#endif
    
    NSMutableString* imageFolder;
}
//------------------------------------------------------------------------------
-(id) initWithContentOfFile:(NSString*)levelFile;
-(id) initWithContentOfFileFromInternet:(NSString*)webAddress;
//url can be a web address / imgFolder needs to be local
-(id) initWithContentOfFileAtURL:(NSURL*)levelURL imagesPath:(NSString*)imgFolder;
-(id) initWithContentOfFile:(NSString*)levelFile 
			 levelSubfolder:(NSString*)levelFolder;
//------------------------------------------------------------------------------

//will call this selector during loading the level (addObjectsToWorld or addSpritesToLayer)
//the registered method needs to have this signature -void loadingProgress:(NSNumber*)percentage
//percentage should be used like this [percentage floatValue] and will return a value from 0.0f to 1.0f
-(void)registerLoadingProgressObserver:(id)object selector:(SEL)selector;

//LOADING
-(void) addSpritesToLayer:(CCLayer*)cocosLayer;
#ifdef LH_USE_BOX2D
-(void) addObjectsToWorld:(b2World*)world cocos2dLayer:(CCLayer*)cocosLayer;
#endif
//------------------------------------------------------------------------------

//UTILITIES
//The offset value is not transformed and is device dependend
//You must apply this offset based on device. To test for the device you eighter use one of the
//ios specific method to get the device or test for the winSize using CCDirector
//for iPhone5 if you want to make the level to be positioned starting from the left-centered corner rather then in the center
//the offset should be CGPointMake(-88, 0)
//for iPad if you want to make the level to be positioned starting from the left-centered corner rather then in the center
//the offset should be CGPointMake(-32,0);
//left-top corner should be CGPointMake(-32,-64);
//THIS METHOD SHOULD BE USED TOGETHER WITH dontStretchArt
+(void) loadLevelsWithOffset:(CGPoint)offset;

+(void) dontStretchArtOnIpad DEPRECATED_ATTRIBUTE;
+(void) dontStretchArt;
+(void) useRetinaOnIpad:(bool)useRet DEPRECATED_ATTRIBUTE;
+(void) useHDonIpad:(bool)useHD;

//------------------------------------------------------------------------------
//PAUSING THE GAME
//this will pause all path movement and all parallaxes
//use  [[CCDirector sharedDirector] pause]; for everything else
+(bool)isPaused;
+(void)setPaused:(bool)value; //pass true to pause, false to unpause

//use this non static methods when you want to pause only one loader and not all
-(bool)isPaused;
-(void)setPaused:(bool)value;

-(LHLayer*)  layerWithUniqueName:(NSString*)name;
-(LHBatch*)  batchWithUniqueName:(NSString*)name;
-(LHSprite*) spriteWithUniqueName:(NSString*)name;
-(LHBezier*) bezierWithUniqueName:(NSString*)name;
-(LHJoint*)  jointWithUniqueName:(NSString*)name;
-(LHParallaxNode*) parallaxNodeWithUniqueName:(NSString*)uniqueName;

-(NSArray*) allLayers;
-(NSArray*) allBatches;
-(NSArray*) allSprites;
-(NSArray*) allBeziers;
-(NSArray*) allJoints;
-(NSArray*) allParallaxes;

-(NSArray*) layersWithTag:(enum LevelHelper_TAG)tag;
-(NSArray*) batchesWithTag:(enum LevelHelper_TAG)tag;
-(NSArray*) spritesWithTag:(enum LevelHelper_TAG)tag;
-(NSArray*) beziersWithTag:(enum LevelHelper_TAG)tag;
-(NSArray*) jointsWithTag:(enum LevelHelper_TAG)tag;


/*
 to remove any of the LHLayer, LHBatch, LHSprite, LHBezier, LHJoint objects call
 
 [object removeSelf];
 
 if you retain it somewhere the object will not be release - so make sure you dont retain
 any of the objects
 
 */

//in cases where you cannot control when the box2d world is deleted
//you may want to call this method prior releasing the LevelHelperLoader instance so that you
//dont get an crash when cocos2d removes the sprites and box2d world object is no longer alive
-(void)removeAllPhysics;




//SPRITE CREATION 
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//name is from one of a sprite already in the level
//parent will be Main Layer
//if you use custom sprite classes - this will create a sprite of that custom registered class
//method will create custom sprite if one is register for the tag of this sprite
-(LHSprite*) createSpriteWithUniqueName:(NSString *)name;

//use this method if you want the sprite to be child of a specific node and not the main LH node
//pass nil if you dont want a parent
//method will create custom sprite if one is register for the tag of this sprite
-(LHSprite*) createSpriteWithUniqueName:(NSString *)name parent:(CCNode*)node;

//name is from one of a sprite already in the level
//parent will be the batch node that is handling the image file of this sprite
//method will create custom sprite if one is register for the tag of this sprite
-(LHSprite*) createBatchSpriteWithUniqueName:(NSString*)name;


-(LHSprite*) createSpriteWithName:(NSString*)name 
                        fromSheet:(NSString*)sheetName
                       fromSHFile:(NSString*)shFileNoExt;

//use this method if you want the sprite to be child of a specific node and not the main LH node
//pass nil if you dont want a parent
-(LHSprite*) createSpriteWithName:(NSString*)name 
                        fromSheet:(NSString*)sheetName
                       fromSHFile:(NSString*)shFileNoExt
                           parent:(CCNode*)node;


//use this in order to create sprites of custom types
-(LHSprite*) createSpriteWithName:(NSString*)name 
                        fromSheet:(NSString*)sheetName
                       fromSHFile:(NSString*)shFileNoExt 
                              tag:(enum LevelHelper_TAG)tag;

//use this method if you want the sprite to be child of a specific node and not the main LH node
//pass nil if you dont want a parent
-(LHSprite*) createSpriteWithName:(NSString*)name 
                        fromSheet:(NSString*)sheetName
                       fromSHFile:(NSString*)shFileNoExt 
                              tag:(enum LevelHelper_TAG)tag
                           parent:(CCNode*)node;


-(LHSprite*) createBatchSpriteWithName:(NSString*)name 
                             fromSheet:(NSString*)sheetName
                            fromSHFile:(NSString*)shFileNoExt;

//use this in order to create sprites of custom types
-(LHSprite*) createBatchSpriteWithName:(NSString*)name 
                             fromSheet:(NSString*)sheetName
                            fromSHFile:(NSString*)shFileNoExt 
                                   tag:(enum LevelHelper_TAG)tag;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
#ifdef LH_USE_BOX2D
//COLLISION HANDLING
//see API Documentation on the website to see how to use this
-(void) useLevelHelperCollisionHandling;

//method will be called twice per fixture, once at start and once at end of the collision".
//because bodies can be formed from multiple fixture method may be called as many times as different fixtures enter in contact.

//e.g. a car enters in collision with a stone, the stone first touched the bumper, (triggers collision 1)
//then the stone enters under the car and touches the under part of the car (trigger collision 2)
-(void) registerBeginOrEndCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA
                                               andTagB:(enum LevelHelper_TAG)tagB
                                            idListener:(id)obj
                                           selListener:(SEL)selector;

-(void) cancelBeginOrEndCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA
                                             andTagB:(enum LevelHelper_TAG)tagB;
              

//this methods will be called durring the lifetime of the collision - many times
-(void) registerPreCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA 
                                       andTagB:(enum LevelHelper_TAG)tagB 
                                    idListener:(id)obj 
                                   selListener:(SEL)selector;

-(void) cancelPreCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA 
                                      andTagB:(enum LevelHelper_TAG)tagB;

-(void) registerPostCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA 
                                        andTagB:(enum LevelHelper_TAG)tagB 
                                     idListener:(id)obj 
                                    selListener:(SEL)selector;

-(void) cancelPostCollisionCallbackBetweenTagA:(enum LevelHelper_TAG)tagA 
                                       andTagB:(enum LevelHelper_TAG)tagB;
#endif
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//sort argument 
//pass NSOrderedAscending -> sort ascending by name
//     NSOrderedSame,     -> no sort - leaves the sprites as they are found in the level
//     NSOrderedDescending-> sort descending by name
-(NSArray*)  spritesWithTag:(enum LevelHelper_TAG)tag ordered:(NSComparisonResult)sortOrder;


//------------------------------------------------------------------------------
-(void) removeParallaxNode:(LHParallaxNode*)node;//does not remove the sprites
-(void) removeParallaxNode:(LHParallaxNode*)node removeChildSprites:(bool)rem;
-(void) removeAllParallaxes; //does not remove the sprites
-(void) removeAllParallaxesAndChildSprites:(bool)remChilds;
//------------------------------------------------------------------------------

//GRAVITY
#ifdef LH_USE_BOX2D
-(bool) isGravityZero;
-(void) createGravity:(b2World*)world;
//------------------------------------------------------------------------------
//PHYSIC BOUNDARIES
-(void) createPhysicBoundaries:(b2World*)_world;

//this method should be used when using dontStretchArtOnIpad
//see api documentatin for more info
-(void) createPhysicBoundariesNoStretching:(b2World *)_world;

-(CGRect) physicBoundariesRect;
-(bool) hasPhysicBoundaries;

-(b2Body*) leftPhysicBoundary;
-(LHNode*) leftPhysicBoundaryNode;
-(LHNode*) leftPhysicBoundarySprite DEPRECATED_ATTRIBUTE;

-(b2Body*) rightPhysicBoundary;
-(LHNode*) rightPhysicBoundaryNode;
-(LHNode*) rightPhysicBoundarySprite DEPRECATED_ATTRIBUTE;

-(b2Body*) topPhysicBoundary;
-(LHNode*) topPhysicBoundaryNode;
-(LHNode*) topPhysicBoundarySprite DEPRECATED_ATTRIBUTE;

-(b2Body*) bottomPhysicBoundary;
-(LHNode*) bottomPhysicBoundaryNode;
-(LHNode*) bottomPhysicBoundarySprite DEPRECATED_ATTRIBUTE;
-(void) removePhysicBoundaries;
#endif
//------------------------------------------------------------------------------
//LEVEL INFO
-(CGSize) gameScreenSize; //the device size set in loaded level
-(CGRect) gameWorldSize; //the size of the game world
//------------------------------------------------------------------------------
//PHYSICS
#ifdef LH_USE_BOX2D
+(void) setMeterRatio:(float)ratio; //default is 32.0f
+(float) meterRatio; //same as pointsToMeterRatio - provided for simplicity as static method

+(float) pixelsToMeterRatio;
+(float) pointsToMeterRatio;

+(b2Vec2) pixelToMeters:(CGPoint)point; //Cocos2d point to Box2d point
+(b2Vec2) pointsToMeters:(CGPoint)point; //Cocos2d point to Box2d point

+(CGPoint) metersToPoints:(b2Vec2)vec; //Box2d point to Cocos2d point
+(CGPoint) metersToPixels:(b2Vec2)vec; //Box2d point to Cocos2d pixels
//------------------------------------------------------------------------------
#endif

//object is of type LHSprite or LHBezier
+(void)setTouchDispatcherForObject:(id)object tag:(int)tag;
+(void)removeTouchDispatcherFromObject:(id)object;
////////////////////////////////////////////////////////////////////////////////
@end






































































