#import <Foundation/Foundation.h>

@interface NSObject(asString)
-(NSString*)asString;
@end

@implementation NSObject(asString)
-(NSString*)asString{
    return([NSString stringWithFormat:@"%@@%p",[self className],self]);}
@end


@interface MclguiKeyedArchiver:NSKeyedArchiver
-(void)encodeObject:(id)object forKey:(NSString*)key;
@end

@implementation MclguiKeyedArchiver
-(void)encodeObject:(id)object forKey:(NSString*)key{
    NSLog(@"%@ encodeObject:%@ forKey:\"%@\"",[self asString],[object asString],key);
    [super encodeObject:object forKey:key];}
@end

id testObject(){
    id d1=[NSMutableDictionary dictionary];
    id d2=[NSMutableDictionary dictionary];
    [d1 setObject:@"Hello" forKey:@"one"];
    [d1 setObject:@"World" forKey:@"two"];
    [d1 setObject:d2 forKey:@"d2"];
    [d2 setObject:@"un" forKey:@"one"];
    [d2 setObject:@"deux" forKey:@"two"];
    [d2 setObject:d1 forKey:@"d1"];
    return(d1);}

void testEncode(){
    id data=[NSMutableData data];
    id archiver=[[MclguiKeyedArchiver alloc] initForWritingWithMutableData:data];
    [archiver encodeObject:testObject() forKey:@"ROOT"];
    [archiver finishEncoding];
    NSLog(@"data=%@",data);
}

int main(){
    id pool=[NSAutoreleasePool new];
    @try{
        testEncode();
    }@finally{
        [pool release];}
    return(0);}
