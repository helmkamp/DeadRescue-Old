//  This file was generated by LevelHelper
//  http://www.levelhelper.org
//
//  LevelHelperLoader.h
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
//
////////////////////////////////////////////////////////////////////////////////

#import <Foundation/Foundation.h>
#import "cocos2d.h"

//notifications
#define LHAnimationHasEndedAllRepetitionsNotification @"LHAnimationHasEndedAllRepetitionsNotification"
#define LHAnimationHasEndedNotification @"LHAnimationHasEndedNotification"
#define LHAnimationFrameNotification @"LHAnimationFrameNotification" //frame notifications are only triggered on frames with user data

//user info keys
#define LHAnimationSpriteObject @"LHAnimationSpriteObject"
#define LHAnimationUserInfo @"LHAnimationUserInfo"
#define LHAnimationFrameName @"LHAnimationFrameName"

@class LHSprite;
@class LHBatch;
@class LHAnimationFrameInfo;
@interface LHAnimationNode : NSObject
{
    NSString* shSceneName;
    NSString* uniqueName;    
    NSString* sheetName;
    NSString* sheetImage;
    float           delayPerUnit;
    NSMutableArray* frames;//array of LHAnimationFrameInfo;
    bool            loop;
    int             repetitions;
    int             repetitionsPerformed;
    bool            restoreOriginalFrame;
    
    LHSprite*       sprite;//the sprite on which this animation obj is assigned to
    
    int             currentFrame;
    float           elapsedFrameTime;
    LHAnimationFrameInfo* activeFrame;
    
    bool            paused;
    
    LHBatch*        oldBatch;
    CCTexture2D*    oldTexture;
    CGRect          oldRect;
    
    CCSpriteFrame*  oldSpriteFrame;
    
    bool frameChanged;
}
@property (readonly) NSString* uniqueName;
@property (readonly) NSString* sheetName;
@property (readonly) NSString* sheetImage;
@property (readonly) NSString* shSceneName;
@property float delayPerUnit;
@property bool  loop;
@property int   repetitions;
@property bool  restoreOriginalFrame;
@property (readonly) LHSprite* sprite;
@property bool  paused;

-(id) initWithDictionary:(NSDictionary*)dictionary 
                onSprite:(LHSprite*)sprite sceneName:(NSString*)scene;

+(id) animationWithDictionary:(NSDictionary*)dic 
                     onSprite:(LHSprite*)sprite sceneName:(NSString*)scene;

-(void)prepare;//sets first frame of the animation as texture of the sprite
-(void)play;
-(void)restart;

-(int)numberOfFrames;
-(void)setFrame:(int)frm;
-(int)currentFrame;

-(void) nextFrame;
-(void) prevFrame;
-(void) nextFrameAndRepeat;
-(void) prevFrameAndRepeat;
-(bool) isAtLastFrame;

-(void)update:(ccTime)dt;

-(void)restoreFrame;//only restore the sprites frame if restoreOriginaFrame is set
////////////////////////////////////////////////////////////////////////////////

-(void)setOldBatch:(LHBatch*)b;
-(void)setOldTexture:(CCTexture2D*)tex;
-(void)setOldRect:(CGRect)r;

-(float)totalTime;

@end	
