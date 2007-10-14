//
//  MuseEnvironment.h
//  muse
//
//  Created by Srikumar Subramanian on 16/03/2007.
//  Copyright 2007 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>

@interface MuseEnvironment : NSObject {
}

+ (void)initialize;

// Key-value coding.
+ (BOOL) accessInstanceVariablesDirectly;
- (id) valueForKey: (NSString*)key;
- (void) setValue: (id)val forKey: (NSString*)key;

@end
