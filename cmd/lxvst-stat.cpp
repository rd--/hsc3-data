#define __cdecl

#include "pluginterfaces/vst2.x/aeffectx.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>

static const VstInt32 kBlockSize = 512;
static const float kSampleRate = 48000.f;
static const VstInt32 kNumProcessCycles = 5;

typedef AEffect* (*PluginEntryProc) (audioMasterCallback audioMaster);
static VstIntPtr VSTCALLBACK vst_callback (AEffect* effect,VstInt32 opcode,VstInt32 index,VstIntPtr value,void* ptr,float opt);

void verify_platform (void)
{
    printf("STAT> SIZEOF VSTINTPTR = %lu\n",sizeof(VstIntPtr));
    printf("STAT> SIZEOF VSTINT32 = %lu\n",sizeof(VstInt32));
    printf("STAT> SIZEOF VOID* = %lu\n",sizeof(void*));
    printf("STAT> SIZEOF AEFFECT = %lu\n",sizeof(AEffect));
    if (sizeof(VstIntPtr) != sizeof(void*)) {
	printf("STAT> PLATFORM VERIFICATION FAILED\n");
	exit(EXIT_FAILURE);
    }
}

static void stat_properties (AEffect* effect);

int main (int argc,char* argv[])
{
    void* module;
    AEffect *effect;
    printf("STAT> VERIFY PLATFORM\n");
    verify_platform();
    printf("STAT> PROCESS ARGUMENTS\n");
    if (argc < 2) {
	printf("STAT> USAGE = LXVST-STAT VST-FILE\n");
	return -1;
    }
    const char* vst_file = argv[1];
    printf("STAT> VST FILE=%s\n",vst_file);
    printf("STAT> LOAD VST LIBRARY\n");
    module = dlopen(vst_file,RTLD_LAZY);
    if (!module) {
	printf("STAT> DLOPEN ERROR: %s\n",dlerror());
	return -1;
    }
    printf("STAT> DLYSM VSTPLUGINMAIN\n");
    PluginEntryProc vst_main = (PluginEntryProc) dlsym(module,"VSTPluginMain");
    if (!vst_main) {
	printf("STAT> NO VSTPLUGINMAIN\n");
	return -1;
    }
    printf("STAT> RUN VSTPLUGINMAIN\n");
    effect = vst_main (vst_callback);
    if (!effect) {
	printf("STAT> VSTPLUGINMAIN FAILED\n");
	return -1;
    }
    printf("STAT> CALL EFFOPEN\n");
    effect->dispatcher (effect,effOpen,0,0,0,0);
    printf("STAT> AUDIO INPUTS = %d\n",effect->numInputs);
    printf("STAT> AUDIO OUTPUTS = %d\n",effect->numOutputs);
    char can_do_midi[64] = "receiveVstMidiEvent";
    printf("STAT> MIDI I/O = %ld\n",effect->dispatcher(effect,effCanDo,0,0,can_do_midi,0));
    stat_properties(effect);
    printf ("STAT> CLOSE EFFECT\n");
    effect->dispatcher(effect,effClose,0,0,0,0);
    printf("STAT> CLOSE MODULE\n");
    dlclose(module);
    return 0;
}

void stat_properties (AEffect* effect)
{
    char effect_name[256] = {0};
    effect->dispatcher (effect,effGetEffectName,0,0,effect_name,0);
    printf ("STAT> EFFECT_NAME = %s\n",effect_name);

    char vendor_string[256] = {0};
    effect->dispatcher (effect,effGetVendorString,0,0,vendor_string,0);
    printf ("STAT> VENDOR_STRING = %s\n",vendor_string);

    char product_string[256] = {0};
    effect->dispatcher (effect,effGetProductString,0,0,product_string,0);
    printf ("STAT> PRODUCT_STRING = %s\n",product_string);

    printf ("STAT> # PARAM = %d\n",effect->numParams);
    for (VstInt32 i = 0; i < effect->numParams; i++) {
	char param_name[256] = {0};
	effect->dispatcher (effect,effGetParamName,i,0,param_name,0);
	printf ("STAT> PARAM %d = %s\n",i,param_name);
    }
}

VstIntPtr VSTCALLBACK
vst_callback (AEffect* e,VstInt32 c,VstInt32 i,VstIntPtr v,void* p,float o)
{
    VstIntPtr result = 0;
    switch (c) {
    case audioMasterVersion :
	result = kVstVersion;
	break;
    }
    return result;
}
