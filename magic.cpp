#include <linux/uinput.h>
#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <fcntl.h>
#include <iostream>
#include "hidapi/hidapi.h"
#include <portmidi.h>

using namespace std;

struct Buttons {
  char hdiHeader;

  bool rawShift : 1;
  bool rawScale : 1;
  bool rawArp : 1;
  bool rawUndo : 1;
  bool rawQuantize : 1;
  bool rawIdeas : 1;
  bool rawLoop : 1;
  bool rawMetro : 1;

  bool rawTempo : 1;
  bool rawPlay : 1;
  bool rawRec : 1;
  bool rawStop : 1;
  bool rawPresetUp : 1;
  bool rawPresetDown : 1;
  bool rawPageLeft : 1;
  bool rawPageRight : 1;

  bool rawBrowser : 1;
  bool rawPlugin : 1;
  bool rawTrack : 1;
  bool rawOctaveDown : 1;
  bool rawOctaveUp : 1;
  bool rawUnknown1 : 1;
  bool rawUnknown2 : 1;
  bool rawUnknown3 : 1;

  bool rawUnknown4 : 1;
  bool rawDial1Touch : 1;
  bool rawDial2Touch : 1;
  bool rawDial3Touch : 1;
  bool rawDial4Touch : 1;
  bool rawDial5Touch : 1;
  bool rawDial6Touch : 1;
  bool rawDial7Touch : 1;

  bool rawDial8Touch : 1;
  bool rawVolPress : 1;
  bool rawUnknown5 : 1;
  bool rawUnknown6 : 1;
  bool rawUnknown7 : 1;
  bool rawUnknown8 : 1;
  bool rawUnknown9 : 1;
  bool rawUnknown10 : 1;

  char unknown1[18];
  unsigned int volume : 8;
  char unknown2[12];
  unsigned int keyshift : 8;
};

void emit(int fd, int type, int code, int val)
{
   struct input_event ie;

   ie.type = type;
   ie.code = code;
   ie.value = val;
   /* timestamp values below are ignored */
   ie.time.tv_sec = 0;
   ie.time.tv_usec = 0;

   write(fd, &ie, sizeof(ie));
}

hid_device* do_hid() {
  int res;
  hid_device *handle;
  int i;

  // Initialize the hidapi library
  res = hid_init();

  // Open the device using the VID, PID,
  // and optionally the Serial number.
  handle = hid_open(0x17cc, 0x1860, NULL);
  return handle;
}

void read_buttons(hid_device* dev, Buttons* buf) {
  hid_read(dev, reinterpret_cast<unsigned char*>(buf), sizeof(Buttons));
}

int main(void) {
  Buttons bs;
  hid_device* dev = do_hid();
  struct uinput_setup usetup;

  int fd = open("/dev/uinput", O_WRONLY | O_NONBLOCK);
  std::cout << fd << std::endl;

  ioctl(fd, UI_SET_EVBIT, EV_KEY);
  ioctl(fd, UI_SET_KEYBIT, KEY_ZENKAKUHANKAKU);
  ioctl(fd, UI_SET_KEYBIT, KEY_KATAKANA);
  ioctl(fd, UI_SET_KEYBIT, KEY_HIRAGANA);
  ioctl(fd, UI_SET_KEYBIT, KEY_SPACE);
  ioctl(fd, UI_SET_KEYBIT, KEY_KATAKANAHIRAGANA);
  ioctl(fd, UI_SET_KEYBIT, KEY_MUHENKAN);
  ioctl(fd, UI_SET_KEYBIT, KEY_PAUSE);
  ioctl(fd, UI_SET_KEYBIT, KEY_KPJPCOMMA);
  ioctl(fd, UI_SET_KEYBIT, KEY_SYSRQ);
  ioctl(fd, UI_SET_KEYBIT, KEY_COMPOSE);
  ioctl(fd, UI_SET_KEYBIT, KEY_STOP);
  ioctl(fd, UI_SET_KEYBIT, KEY_UNDO);
  ioctl(fd, UI_SET_KEYBIT,KEY_ZENKAKUHANKAKU) ;
  ioctl(fd, UI_SET_KEYBIT,KEY_KATAKANA) ;
  ioctl(fd, UI_SET_KEYBIT,KEY_AGAIN);
  ioctl(fd, UI_SET_KEYBIT,KEY_HIRAGANA);
  ioctl(fd, UI_SET_KEYBIT,KEY_SETUP);
  ioctl(fd, UI_SET_KEYBIT,KEY_KATAKANAHIRAGANA);
  ioctl(fd, UI_SET_KEYBIT,KEY_PAUSE);
  ioctl(fd, UI_SET_KEYBIT,KEY_KPJPCOMMA);
  ioctl(fd, UI_SET_KEYBIT,KEY_SYSRQ);
  ioctl(fd, UI_SET_KEYBIT,KEY_MUHENKAN);
  ioctl(fd, UI_SET_KEYBIT,KEY_SPACE);

  memset(&usetup, 0, sizeof(usetup));
  usetup.id.bustype = BUS_USB;
  usetup.id.vendor = 0x1234; /* sample vendor */
  usetup.id.product = 0x5678; /* sample product */
  strcpy(usetup.name, "Example device");

  ioctl(fd, UI_DEV_SETUP, &usetup);
  ioctl(fd, UI_DEV_CREATE);

  sleep(1);


   for (;;) {
    read_buttons(dev, &bs);

#define SEND(name, key) emit(fd, EV_KEY, KEY_##key, bs.raw##name);
    SEND(Scale, COMPOSE)
    SEND(Arp, STOP)
    SEND(Undo, UNDO)
    SEND(Quantize,ZENKAKUHANKAKU)
    SEND(Ideas,KATAKANA)
    SEND(Loop,AGAIN)
    SEND(Metro,HIRAGANA)
    SEND(Tempo,SETUP)
    SEND(Play,SPACE)
    SEND(Rec,KATAKANAHIRAGANA)
    SEND(Stop,PAUSE)
    SEND(Browser,KPJPCOMMA)
    emit(fd, EV_KEY, KEY_SPACE, bs.rawPlugin);
    SEND(Track,MUHENKAN)

    emit(fd, EV_SYN, SYN_REPORT, 0);
   }

   sleep(1);

   ioctl(fd, UI_DEV_DESTROY);
   close(fd);

   return 0;
}

