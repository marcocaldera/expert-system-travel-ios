//
//  Function.m
//  TRAGEX
//
//  Created by Marco Caldera on 20/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

#import "Function.h"
#import <CLIPSiOS/clips.h>

@implementation Function

-(struct multifield *)isUserModelValid:(DATA_OBJECT)param1 {
    struct multifield *test = GetValue(param1);
    return test;
//    return @"cio zio";
}

@end
