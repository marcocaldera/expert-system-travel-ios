//
//  Function.m
//  TRAGEX
//
//  Created by Marco Caldera on 20/09/2019.
//  Copyright © 2019 Marco Caldera. All rights reserved.
//

#import "Function.h"
#import <CLIPSiOS/clips.h>
#import "TRAGEX-Swift.h"


@implementation Function

-(NSMutableArray *)isUserModelValid:(void *) clipsEnv result:(DATA_OBJECT) result {
    struct multifield *theMultifield;
    void *theTripFact;
    DATA_OBJECT theSlot;
    const char *theString;
    NSMutableArray<Trip *> *best_trips = [NSMutableArray new];
    struct multifield *trip_list = GetValue(result);
    for (int i = 1; i <= GetDOLength(result); i++){
        
        //Prelevo l'elemento trip all'indice i corrente
        theTripFact = GetMFValue(trip_list,i);
        Trip *trip = [[Trip alloc] init];//Alloco un nuovo trip
        
        //Prelevo la certainties
        EnvGetFactSlot(clipsEnv, theTripFact, "certainties", &theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);//Prendo il valore delo slot che è un multifield
            //Certainties è un multislot ma contiene un solo elemento
            NSNumber *certainty = [NSNumber numberWithFloat: ValueToDouble(GetMFValue(theMultifield,1))];
            float floatvalue = [certainty floatValue];
//            NSLog (@"CF = %f", floatvalue);
            trip.certainties = floatvalue;//Aggiungo la certainty al trip
            
        }
        
        // Prelevo la place-sequence
        EnvGetFactSlot(clipsEnv,theTripFact,"place-sequence",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);//Prendo il valore dello slot ottenuto (che contiene un multifield)
            NSMutableArray *tempArray = [[NSMutableArray alloc] init];
            //Ciclo sul multifield
            for (int j = 1; j <= GetDOLength(theSlot); j++){
//                EnvPrintRouter(clipsEnv,WPROMPT,ValueToString(GetMFValue(theMultifield, j)));
                theString = ValueToString(GetMFValue(theMultifield, j));
                [tempArray addObject: [NSString stringWithCString: theString encoding: NSUTF8StringEncoding].capitalizedString];
                
            }
            trip.placeSequence = tempArray;
        }
        
        // Prelevo la resort-sequence
        EnvGetFactSlot(clipsEnv,theTripFact,"resort-sequence",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            NSMutableArray *tempArray = [[NSMutableArray alloc] init];
            for (int j = 1; j <= GetDOLength(theSlot); j++){
//                EnvPrintRouter(clipsEnv,WPROMPT,ValueToString(GetMFValue(theMultifield, j)));
                theString = ValueToString(GetMFValue(theMultifield, j));
                [tempArray addObject: [NSString stringWithCString: theString encoding: NSUTF8StringEncoding].capitalizedString];
            }
            trip.resortSequence = tempArray;
        }
        
        // Prelevo la price-per-night
        EnvGetFactSlot(clipsEnv,theTripFact,"price-per-night",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            NSMutableArray *tempArray = [[NSMutableArray alloc] init];
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                NSNumber *certainty = [NSNumber numberWithInteger: ValueToInteger(GetMFValue(theMultifield,j))];
                long longvalue = [certainty integerValue];
//                NSLog (@"Prezzo = %ld", longvalue);
                [tempArray addObject:[NSNumber numberWithInteger: longvalue]];
            }
            trip.pricePerNight = tempArray;
        }
        
        // Prelevo la days-distribution
        EnvGetFactSlot(clipsEnv,theTripFact,"days-distribution",&theSlot);
        if (GetType(theSlot) == MULTIFIELD) {
            theMultifield = GetValue(theSlot);
            NSMutableArray *tempArray = [[NSMutableArray alloc] init];
            for (int j = 1; j <= GetDOLength(theSlot); j++){
                NSNumber *certainty = [NSNumber numberWithInteger: ValueToInteger(GetMFValue(theMultifield,j))];
                long longvalue = [certainty integerValue];
//                NSLog (@"Giorni = %ld", longvalue);
                [tempArray addObject:[NSNumber numberWithInteger: longvalue]];
            }
            trip.daysDistributions = tempArray;
        }
        
        [best_trips addObject:trip];
        
    }
    
    return best_trips;
//    return @"cio zio";
}

@end
