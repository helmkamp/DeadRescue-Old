//
//  HelloWorldLayer.m
//  TileGame
//
//  Created by Andrew Helmkamp on 12/15/12.
//  Copyright __MyCompanyName__ 2012. All rights reserved.
//


// Import the interfaces
#import "HelloWorldLayer.h"

// Needed to obtain the Navigation Controller
#import "AppDelegate.h"

@implementation HelloWorldHub

-(id) init {
    if ( (self = [super init])) {
        CGSize winSize = [[CCDirector sharedDirector] winSize];
        label = [CCLabelTTF labelWithString:@"0" dimensions:CGSizeMake(50, 20) hAlignment:UITextAlignmentRight fontName:@"Verdana-Bold" fontSize:18.0];
        label.color = ccc3(0, 0, 0);
        int margin = 10;
        label.position = ccp(winSize.width - (label.contentSize.width/2) - margin, label.contentSize.height/2 + margin);
        [self addChild:label];
    }
    return self;
}

-(void)numCollectedChanged:(int)numCollected {
    [label setString:[NSString stringWithFormat:@"%d", numCollected]];
}

@end

#pragma mark - HelloWorldLayer

// HelloWorldLayer implementation
@implementation HelloWorldLayer

@synthesize player = _player, numCollected = _numCollected, hud = _hud;

// Helper class method that creates a Scene with the HelloWorldLayer as the only child.
+(CCScene *) scene
{
	// 'scene' is an autorelease object.
	CCScene *scene = [CCScene node];
	
	// 'layer' is an autorelease object.
	HelloWorldLayer *layer = [HelloWorldLayer node];
	
	// add layer as a child to scene
	[scene addChild: layer];
    
    //add the score layer
    HelloWorldHub *hud = [HelloWorldHub node];
    [scene addChild:hud];
    layer.hud = hud;
	
	// return the scene
	return scene;
}

// on "init" you need to initialize your instance
-(id) init
{
	// always call "super" init
	// Apple recommends to re-assign "self" with the "super's" return value
	if( (self=[super init]) ) {
        self.isTouchEnabled = YES;
        
        //Add the background
		loader = [[LevelHelperLoader alloc] initWithContentOfFile:@"level3"];
        [loader addSpritesToLayer:self];
        
        levelSize = [loader gameWorldSize];
		
		//Add the player
        self.player = [CCSprite spriteWithFile:@"Player.png"];
        _player.position = ccp(150,150);
        
        //Add spritesheet
        //[[CCSpriteFrameCache sharedSpriteFrameCache] addSpriteFramesWithFile:@"zombieWalk_UntitledSheet.plist"];
        
        //Add a zombie and animation
//        CCSprite *zombie = [CCSprite spriteWithSpriteFrameName:@"zombie_walk_01"];
//        zombie.position = ccp(240,320);
//        zombie.flipX = YES;
//        [self addChild:zombie];
//        zombie.tag = 1;
//        
//        NSMutableArray *frames = [[NSMutableArray alloc] init];
//        for (int i = 1; i<=4; i++) {
//            NSString *frameName = [NSString stringWithFormat:@"zombie_walk_0%i", i];
//            [frames addObject:[[CCSpriteFrameCache sharedSpriteFrameCache] spriteFrameByName:frameName]];
//        }
//        
//        CCAnimation *a = [CCAnimation animationWithSpriteFrames:frames delay:1.0f];
//        [zombie runAction:[CCRepeatForever actionWithAction:[CCSpawn actions: [CCAnimate actionWithAnimation:a],
//                                                             [CCCallFunc actionWithTarget:self selector:@selector(moveZombie)],   
//                                                             nil]]];
        
        
        [self addChild:_player];
        
//        [self setViewpointCenter:_player.position];
		
		
        
	}
	return self;
}

-(void)moveZombie {
    [CCSequence actions:[CCMoveTo actionWithDuration:8 position:ccp(140,320)],
//     [CCCallBlock actionWithBlock:^{
//        zombie.flipX = NO;
//    }],
     [CCMoveTo actionWithDuration:8 position:ccp(240,320)],
//     [CCCallBlock actionWithBlock:^{
//        zombie.flipX = YES;
//        }],
     nil];
}

-(CGPoint) tileCoordForPosition:(CGPoint)position {
    int x = position.x / levelSize.size.width;
    int y = ((levelSize.size.height * levelSize.size.height)-position.y) / levelSize.size.height;
    return ccp(x,y);
}
//
-(void)setViewpointCenter:(CGPoint) position {
    // ask director for the window size
    CGSize size = [[CCDirector sharedDirector] winSize];
    
    int x = MAX(position.x, size.width/2);
    int y = MAX(position.y, size.height/2);
    
    x = MIN(x, (levelSize.size.width * levelSize.size.width) - size.width/2);
    y = MIN(y, (levelSize.size.height * levelSize.size.height) - size.height/2);
    
    CGPoint actualPosition = ccp(x, y);
    
    CGPoint centerOfView = ccp(size.width/2, size.height/2);
    CGPoint viewPoint = ccpSub(centerOfView, actualPosition);
    self.position = viewPoint;
}

-(void)registerWithTouchDispatcher {
    [[[CCDirector sharedDirector] touchDispatcher] addTargetedDelegate:self priority:0 swallowsTouches:YES];
}

-(BOOL) ccTouchBegan:(UITouch *)touch withEvent:(UIEvent *)event {
    return YES;
}

-(void)setPlayerPosition:(CGPoint)position {
    CGPoint tileCoord = [self tileCoordForPosition:position];
    
    _player.position = position;
}

-(void)ccTouchEnded:(UITouch *)touch withEvent:(UIEvent *)event {
    CGPoint touchLocation = [touch locationInView:[touch view]];
    touchLocation = [[CCDirector sharedDirector] convertToGL: touchLocation];
    touchLocation = [self convertToNodeSpace:touchLocation];
    
    _player.position = touchLocation;
    
    CGPoint playerPos = _player.position;
    CGPoint diff = ccpSub(touchLocation, playerPos);
    if (abs(diff.x) > abs(diff.y)) {
        if (diff.x > 0) {
            playerPos.x += levelSize.size.width;
        } else {
            playerPos.x -= levelSize.size.width;
        }
    } else {
        if (diff.y > 0) {
            playerPos.y += levelSize.size.height;
        } else {
            playerPos.y -= levelSize.size.height;
        }
    }
    
    if (playerPos.x <= (levelSize.size.width * levelSize.size.width) && playerPos.y <= (levelSize.size.height * levelSize.size.height) && playerPos.y >= 0 && playerPos.x >= 0) {
        [self setPlayerPosition:playerPos];
    }
    
    [self setViewpointCenter:_player.position];
}










// on "dealloc" you need to release all your retained objects
- (void) dealloc
{
    self.player = nil;
    self.hud = nil;
	
	// don't forget to call "super dealloc"
	[super dealloc];
}












@end
