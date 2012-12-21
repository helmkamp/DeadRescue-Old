//
//  HelloWorldLayer.h
//  TileGame
//
//  Created by Andrew Helmkamp on 12/15/12.
//  Copyright __MyCompanyName__ 2012. All rights reserved.
//


#import <GameKit/GameKit.h>

// When you import this file, you import all the cocos2d classes
#import "cocos2d.h"
#import "LevelHelperLoader.h"

@interface HelloWorldHub : CCLayer {
    CCLabelTTF *label;
}

-(void)numCollectedChanged:(int)numCollected;

@end


// HelloWorldLayer
@interface HelloWorldLayer : CCLayer 
{
    LevelHelperLoader *loader;
    CCSprite *_player;
    
    CGRect levelSize;
    
    int _numCollected;
    HelloWorldHub *_hud;
}

// returns a CCScene that contains the HelloWorldLayer as the only child
+(CCScene *) scene;


@property (nonatomic, retain) CCSprite *player;

@property (nonatomic, assign) int numCollected;
@property (nonatomic, retain) HelloWorldHub *hud;

@end
