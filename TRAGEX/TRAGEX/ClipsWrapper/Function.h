//
//  Function.h
//  TRAGEX
//
//  Created by Marco Caldera on 20/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CLIPSiOS/clips.h>

NS_ASSUME_NONNULL_BEGIN

@interface Function : NSObject

-(struct multifield *)isUserModelValid:(void *) clipsEnv result:(DATA_OBJECT) result;

@end

NS_ASSUME_NONNULL_END
