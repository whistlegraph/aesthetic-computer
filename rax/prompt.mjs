// Keyboard text entry rendered by the same GPU scene as the world.
export class Prompt {
  constructor() {
    this.open = false;
    this.text = '';
    this.cursor = 0;
    this.target = null;
    this.message = '';
    this.busy = false;
  }
  begin(target = null) {
    this.open = true;
    this.target = target;
    this.message = '';
    this.cursor = this.text.length;
  }
  key(event) {
    if (!this.open) return false;
    const { key, ctrlKey, metaKey, altKey } = event;
    if (key === 'Escape') {
      this.open = false;
      return true;
    }
    if (key === 'ArrowLeft') this.cursor = Math.max(0, this.cursor - 1);
    else if (key === 'ArrowRight') this.cursor = Math.min(this.text.length, this.cursor + 1);
    else if (key === 'Home') this.cursor = 0;
    else if (key === 'End') this.cursor = this.text.length;
    else if (key === 'Backspace' && this.cursor) {
      this.text = this.text.slice(0, this.cursor - 1) + this.text.slice(this.cursor);
      this.cursor--;
    } else if (key === 'Delete')
      this.text = this.text.slice(0, this.cursor) + this.text.slice(this.cursor + 1);
    else if (key.length === 1 && !ctrlKey && !metaKey && !altKey) this.insert(key);
    return true;
  }
  insert(text) {
    const addition = text.replace(/[^\x20-\x7e]/g, ' ').slice(0, 280 - this.text.length);
    this.text = this.text.slice(0, this.cursor) + addition + this.text.slice(this.cursor);
    this.cursor += addition.length;
  }
}
