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

-(struct multifield *)isUserModelValid:(void *) clipsEnv result:(DATA_OBJECT) result {
    struct multifield *theMultifield;
    void *theTripFact;
    struct multifield *trip_list = GetValue(result);
    DATA_OBJECT theSlot;
    for (int i = 1; i <= GetDOLength(result); i++){
        
        //Prelevo l'elemento trip all'indice i corrente
        theTripFact = GetMFValue(trip_list,i);
        
        //Prelevo la certainties
        EnvGetFactSlot(clipsEnv, theTripFact, "certainties", &theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            //Certainties è un multislot ma contiene un solo elemento
            NSNumber *certainty = [NSNumber numberWithFloat: ValueToDouble(GetMFValue(theMultifield,1))];
            float floatvalue = [certainty floatValue];
            NSLog (@"CF = %f", floatvalue);
        }
        
        // Prelevo la place-sequence
        EnvGetFactSlot(clipsEnv,theTripFact,"place-sequence",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                EnvPrintRouter(clipsEnv,WPROMPT,ValueToString(GetMFValue(theMultifield, j)));
            }
        }
        
        // Prelevo la resort-sequence
        EnvGetFactSlot(clipsEnv,theTripFact,"resort-sequence",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                EnvPrintRouter(clipsEnv,WPROMPT,ValueToString(GetMFValue(theMultifield, j)));
            }
        }
        
        // Prelevo la price-per-night
        EnvGetFactSlot(clipsEnv,theTripFact,"price-per-night",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                NSNumber *certainty = [NSNumber numberWithInteger: ValueToInteger(GetMFValue(theMultifield,j))];
                long longvalue = [certainty integerValue];
                NSLog (@"Prezzo = %ld", longvalue);
            }
        }
        
        // Prelevo la days-distribution
        EnvGetFactSlot(clipsEnv,theTripFact,"days-distribution",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                NSNumber *certainty = [NSNumber numberWithInteger: ValueToInteger(GetMFValue(theMultifield,j))];
                long longvalue = [certainty integerValue];
                NSLog (@"Giorni = %ld", longvalue);
            }
        }
        
    }
    
    return trip_list;
//    return @"cio zio";
}

@end
