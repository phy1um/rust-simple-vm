
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

