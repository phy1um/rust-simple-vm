const COLOURS = [
  [0,0,0],
  [205,49,49],
  [13,188,121],
  [229,229,16],
  [36,114,200],
  [188,63,188],
  [17,168,205],
  [229,229,229],
  [102,102,102],
  [241,76,76],
  [35,209,139],
  [245,245,67],
  [59,142,234],
  [214,112,214],
  [41,184,219],
  [255,255,255],
];

class Monitor {
  constructor(pixelWidth, pixelHeight) {
    this._w = pixelWidth;
    this._h = pixelHeight;
    this._modeTiled = false;
    this._bpp = 8;
    this._buffer = new Uint8Array(this.screenBufferByteSize());
    this._img = new ImageData(pixelWidth, pixelHeight);
    this._innerCanvas = document.createElement("canvas");
    this._innerCanvas.width = pixelWidth;
    this._innerCanvas.height = pixelHeight;
    this._innerCanvas.style.imageRendering =  "pixel-perfect";
  }

  clear() {
    for (let i = 0; i < this._buffer.length; i++) {
      this._buffer[i] = 0;
    }
  }

  screenBufferByteSize() {
    if (this._modeTiled) {
      console.error("unimplemented");
      return 0;
    } else {
      return Math.floor(this._w * this._h * (this._bpp/8));
    }
  }

  mapBufferTo(vm, addr) {
    vm.map_memory_func(
      addr, 
      this.screenBufferByteSize(), 
      (a) => this._read(a),
      (a, v) => this._write(a, v),
    );
  }

  _read(addr) {
    return this._buffer[addr];
  }

  _write(addr, value) {
    this._buffer[addr] = value;
  }

  _getPal(i) {
    return COLOURS[i%COLOURS.length];
  }

  _renderToImage() {
    for (let y = 0; y < this._h; y++) {
      for (let x = 0; x < this._w; x++) {
        if (this._bpp == 8) {
          const i = y*this._w + x;
          const col = this._getPal(this._buffer[i]);
          this._img.data[4*i + 0] = col[0];
          this._img.data[4*i + 1] = col[1];
          this._img.data[4*i + 2] = col[2];
          this._img.data[4*i + 3] = 255;
        } else if (this._bpp == 4) {
          const i = y*this._w + x;
          const local_index = y*Math.floor(this._w/2) + Math.floor(x/2);
          let bufferByte = this._buffer[local_index];
          let nibble = x%2==0 ? bufferByte&0xf : (bufferByte>>4)&0xf;
          const col = this._getPal(nibble);
          this._img.data[4*i + 0] = col[0];
          this._img.data[4*i + 1] = col[1];
          this._img.data[4*i + 2] = col[2];
          this._img.data[4*i + 3] = 255;
        } else {
          throw new Error(`unimplemented bpp: ${this._bpp}`);
        }
      }
    }
  }

  getImage() {
    this._renderToImage();
    this._innerCanvas.getContext("2d").putImageData(this._img, 0, 0);
    return this._innerCanvas;
  }
}

class TextIO {
  constructor() {
    this._outBuffer = "$> ";
    this._inBuffer = "";
    this._renderTarget = document.createElement("pre");
    this._readTarget = document.createElement("input");
    this.refreshOutput(); }

  getOutputElement() {
    return this._renderTarget;
  }

  getInputElement() {
    return this._readTarget;
  }
  
  getInputButton() {
    const b = document.createElement("a");
    b.role = "button";
    b.innerText = "Send";
    b.onclick = () => {
      this._inBuffer += this._readTarget.value;
      this._readTarget.value = "";
    }
    return b;
  }

  refreshOutput() {
    this._renderTarget.innerText = this._outBuffer;
  }

  write(b) {
    this._outBuffer += b;
    this.refreshOutput();
  }

  read() {
    if (this._inBuffer.length > 0) {
      const out = this._inBuffer.charCodeAt(0);
      this._inBuffer = this._inBuffer.substring(1);
      return out;
    } else {
      return 0;
    }
  }

  bind(vm, offset) {
    vm.map_memory_func(offset, 2, 
      // read function
      (addr) => {
        if (addr == 0) {
          return 0; 
        } else if (addr == 1) {
          return this.read();
        } else {
          throw new Exception("read OOB");
        }
      }, 
      // write function
      (addr, value) => {
        if (addr == 0) {
          // todo: turn value into a char
          this.write(String.fromCharCode(value));
        } else if (addr == 1) {
          return;
        } else {
          throw new Exception("write OOB");
        }
      },
    )
  }
}

class LoggingMemory {
  constructor(size) {
    this._b = new Uint8Array(size);
    this._loud = true;
  }

  setLog(b) {
    this._loud = b;
  }

  reader() {
    return (addr) => {
      if (this._loud) {
        console.log(`read @ 0x${addr.toString(16)}`);
      }
      return this._b[addr];
    }
  }

  writer() {
    return (addr, value) => {
      if (this._loud) {
        console.log(`write ${value.toString(16)} @ 0x${addr.toString(16)}`);
      }
      this._b[addr] = value;
    }
  }
}

