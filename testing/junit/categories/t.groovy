String a = "org.auraframework.def.DefinitionTest"
String b = "aura-impl/src/test/java/org/auraframework/def/DefinitionTest.java"
//String  = 

String testSub = "src/test/java/"
b = b[b.indexOf(testSub) + testSub.size() .. b.size()-1]
String className = b.lastIndexOf('.').with {it != -1 ? b[0..<it] : b}
className = className.replaceAll( '/', '.' )

println a
println className

assert a == className
