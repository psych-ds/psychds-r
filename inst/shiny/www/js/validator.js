var __defProp = Object.defineProperty;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __esm = (fn, res) => function __init() {
  return fn && (res = (0, fn[__getOwnPropNames(fn)[0]])(fn = 0)), res;
};
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __accessCheck = (obj, member, msg) => {
  if (!member.has(obj))
    throw TypeError("Cannot " + msg);
};
var __privateGet = (obj, member, getter) => {
  __accessCheck(obj, member, "read from private field");
  return getter ? getter.call(obj) : member.get(obj);
};
var __privateAdd = (obj, member, value) => {
  if (member.has(obj))
    throw TypeError("Cannot add the same private member more than once");
  member instanceof WeakSet ? member.add(obj) : member.set(obj, value);
};
var __privateSet = (obj, member, value, setter) => {
  __accessCheck(obj, member, "write to private field");
  setter ? setter.call(obj, value) : member.set(obj, value);
  return value;
};

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/dist/esm/sync.js
var sync_exports = {};
__export(sync_exports, {
  CsvError: () => CsvError,
  parse: () => parse
});
function init() {
  inited = true;
  var code = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
  for (var i = 0, len = code.length; i < len; ++i) {
    lookup[i] = code[i];
    revLookup[code.charCodeAt(i)] = i;
  }
  revLookup["-".charCodeAt(0)] = 62;
  revLookup["_".charCodeAt(0)] = 63;
}
function toByteArray(b64) {
  if (!inited) {
    init();
  }
  var i, j, l, tmp, placeHolders, arr;
  var len = b64.length;
  if (len % 4 > 0) {
    throw new Error("Invalid string. Length must be a multiple of 4");
  }
  placeHolders = b64[len - 2] === "=" ? 2 : b64[len - 1] === "=" ? 1 : 0;
  arr = new Arr(len * 3 / 4 - placeHolders);
  l = placeHolders > 0 ? len - 4 : len;
  var L = 0;
  for (i = 0, j = 0; i < l; i += 4, j += 3) {
    tmp = revLookup[b64.charCodeAt(i)] << 18 | revLookup[b64.charCodeAt(i + 1)] << 12 | revLookup[b64.charCodeAt(i + 2)] << 6 | revLookup[b64.charCodeAt(i + 3)];
    arr[L++] = tmp >> 16 & 255;
    arr[L++] = tmp >> 8 & 255;
    arr[L++] = tmp & 255;
  }
  if (placeHolders === 2) {
    tmp = revLookup[b64.charCodeAt(i)] << 2 | revLookup[b64.charCodeAt(i + 1)] >> 4;
    arr[L++] = tmp & 255;
  } else if (placeHolders === 1) {
    tmp = revLookup[b64.charCodeAt(i)] << 10 | revLookup[b64.charCodeAt(i + 1)] << 4 | revLookup[b64.charCodeAt(i + 2)] >> 2;
    arr[L++] = tmp >> 8 & 255;
    arr[L++] = tmp & 255;
  }
  return arr;
}
function tripletToBase64(num) {
  return lookup[num >> 18 & 63] + lookup[num >> 12 & 63] + lookup[num >> 6 & 63] + lookup[num & 63];
}
function encodeChunk(uint8, start, end) {
  var tmp;
  var output = [];
  for (var i = start; i < end; i += 3) {
    tmp = (uint8[i] << 16) + (uint8[i + 1] << 8) + uint8[i + 2];
    output.push(tripletToBase64(tmp));
  }
  return output.join("");
}
function fromByteArray(uint8) {
  if (!inited) {
    init();
  }
  var tmp;
  var len = uint8.length;
  var extraBytes = len % 3;
  var output = "";
  var parts = [];
  var maxChunkLength = 16383;
  for (var i = 0, len2 = len - extraBytes; i < len2; i += maxChunkLength) {
    parts.push(encodeChunk(uint8, i, i + maxChunkLength > len2 ? len2 : i + maxChunkLength));
  }
  if (extraBytes === 1) {
    tmp = uint8[len - 1];
    output += lookup[tmp >> 2];
    output += lookup[tmp << 4 & 63];
    output += "==";
  } else if (extraBytes === 2) {
    tmp = (uint8[len - 2] << 8) + uint8[len - 1];
    output += lookup[tmp >> 10];
    output += lookup[tmp >> 4 & 63];
    output += lookup[tmp << 2 & 63];
    output += "=";
  }
  parts.push(output);
  return parts.join("");
}
function read(buffer, offset, isLE, mLen, nBytes) {
  var e, m;
  var eLen = nBytes * 8 - mLen - 1;
  var eMax = (1 << eLen) - 1;
  var eBias = eMax >> 1;
  var nBits = -7;
  var i = isLE ? nBytes - 1 : 0;
  var d = isLE ? -1 : 1;
  var s = buffer[offset + i];
  i += d;
  e = s & (1 << -nBits) - 1;
  s >>= -nBits;
  nBits += eLen;
  for (; nBits > 0; e = e * 256 + buffer[offset + i], i += d, nBits -= 8) {
  }
  m = e & (1 << -nBits) - 1;
  e >>= -nBits;
  nBits += mLen;
  for (; nBits > 0; m = m * 256 + buffer[offset + i], i += d, nBits -= 8) {
  }
  if (e === 0) {
    e = 1 - eBias;
  } else if (e === eMax) {
    return m ? NaN : (s ? -1 : 1) * Infinity;
  } else {
    m = m + Math.pow(2, mLen);
    e = e - eBias;
  }
  return (s ? -1 : 1) * m * Math.pow(2, e - mLen);
}
function write(buffer, value, offset, isLE, mLen, nBytes) {
  var e, m, c;
  var eLen = nBytes * 8 - mLen - 1;
  var eMax = (1 << eLen) - 1;
  var eBias = eMax >> 1;
  var rt = mLen === 23 ? Math.pow(2, -24) - Math.pow(2, -77) : 0;
  var i = isLE ? 0 : nBytes - 1;
  var d = isLE ? 1 : -1;
  var s = value < 0 || value === 0 && 1 / value < 0 ? 1 : 0;
  value = Math.abs(value);
  if (isNaN(value) || value === Infinity) {
    m = isNaN(value) ? 1 : 0;
    e = eMax;
  } else {
    e = Math.floor(Math.log(value) / Math.LN2);
    if (value * (c = Math.pow(2, -e)) < 1) {
      e--;
      c *= 2;
    }
    if (e + eBias >= 1) {
      value += rt / c;
    } else {
      value += rt * Math.pow(2, 1 - eBias);
    }
    if (value * c >= 2) {
      e++;
      c /= 2;
    }
    if (e + eBias >= eMax) {
      m = 0;
      e = eMax;
    } else if (e + eBias >= 1) {
      m = (value * c - 1) * Math.pow(2, mLen);
      e = e + eBias;
    } else {
      m = value * Math.pow(2, eBias - 1) * Math.pow(2, mLen);
      e = 0;
    }
  }
  for (; mLen >= 8; buffer[offset + i] = m & 255, i += d, m /= 256, mLen -= 8) {
  }
  e = e << mLen | m;
  eLen += mLen;
  for (; eLen > 0; buffer[offset + i] = e & 255, i += d, e /= 256, eLen -= 8) {
  }
  buffer[offset + i - d] |= s * 128;
}
function kMaxLength() {
  return Buffer2.TYPED_ARRAY_SUPPORT ? 2147483647 : 1073741823;
}
function createBuffer(that, length) {
  if (kMaxLength() < length) {
    throw new RangeError("Invalid typed array length");
  }
  if (Buffer2.TYPED_ARRAY_SUPPORT) {
    that = new Uint8Array(length);
    that.__proto__ = Buffer2.prototype;
  } else {
    if (that === null) {
      that = new Buffer2(length);
    }
    that.length = length;
  }
  return that;
}
function Buffer2(arg, encodingOrOffset, length) {
  if (!Buffer2.TYPED_ARRAY_SUPPORT && !(this instanceof Buffer2)) {
    return new Buffer2(arg, encodingOrOffset, length);
  }
  if (typeof arg === "number") {
    if (typeof encodingOrOffset === "string") {
      throw new Error(
        "If encoding is specified then the first argument must be a string"
      );
    }
    return allocUnsafe(this, arg);
  }
  return from(this, arg, encodingOrOffset, length);
}
function from(that, value, encodingOrOffset, length) {
  if (typeof value === "number") {
    throw new TypeError('"value" argument must not be a number');
  }
  if (typeof ArrayBuffer !== "undefined" && value instanceof ArrayBuffer) {
    return fromArrayBuffer(that, value, encodingOrOffset, length);
  }
  if (typeof value === "string") {
    return fromString(that, value, encodingOrOffset);
  }
  return fromObject(that, value);
}
function assertSize(size) {
  if (typeof size !== "number") {
    throw new TypeError('"size" argument must be a number');
  } else if (size < 0) {
    throw new RangeError('"size" argument must not be negative');
  }
}
function alloc(that, size, fill2, encoding) {
  assertSize(size);
  if (size <= 0) {
    return createBuffer(that, size);
  }
  if (fill2 !== void 0) {
    return typeof encoding === "string" ? createBuffer(that, size).fill(fill2, encoding) : createBuffer(that, size).fill(fill2);
  }
  return createBuffer(that, size);
}
function allocUnsafe(that, size) {
  assertSize(size);
  that = createBuffer(that, size < 0 ? 0 : checked(size) | 0);
  if (!Buffer2.TYPED_ARRAY_SUPPORT) {
    for (var i = 0; i < size; ++i) {
      that[i] = 0;
    }
  }
  return that;
}
function fromString(that, string, encoding) {
  if (typeof encoding !== "string" || encoding === "") {
    encoding = "utf8";
  }
  if (!Buffer2.isEncoding(encoding)) {
    throw new TypeError('"encoding" must be a valid string encoding');
  }
  var length = byteLength(string, encoding) | 0;
  that = createBuffer(that, length);
  var actual = that.write(string, encoding);
  if (actual !== length) {
    that = that.slice(0, actual);
  }
  return that;
}
function fromArrayLike(that, array) {
  var length = array.length < 0 ? 0 : checked(array.length) | 0;
  that = createBuffer(that, length);
  for (var i = 0; i < length; i += 1) {
    that[i] = array[i] & 255;
  }
  return that;
}
function fromArrayBuffer(that, array, byteOffset, length) {
  array.byteLength;
  if (byteOffset < 0 || array.byteLength < byteOffset) {
    throw new RangeError("'offset' is out of bounds");
  }
  if (array.byteLength < byteOffset + (length || 0)) {
    throw new RangeError("'length' is out of bounds");
  }
  if (byteOffset === void 0 && length === void 0) {
    array = new Uint8Array(array);
  } else if (length === void 0) {
    array = new Uint8Array(array, byteOffset);
  } else {
    array = new Uint8Array(array, byteOffset, length);
  }
  if (Buffer2.TYPED_ARRAY_SUPPORT) {
    that = array;
    that.__proto__ = Buffer2.prototype;
  } else {
    that = fromArrayLike(that, array);
  }
  return that;
}
function fromObject(that, obj) {
  if (internalIsBuffer(obj)) {
    var len = checked(obj.length) | 0;
    that = createBuffer(that, len);
    if (that.length === 0) {
      return that;
    }
    obj.copy(that, 0, 0, len);
    return that;
  }
  if (obj) {
    if (typeof ArrayBuffer !== "undefined" && obj.buffer instanceof ArrayBuffer || "length" in obj) {
      if (typeof obj.length !== "number" || isnan(obj.length)) {
        return createBuffer(that, 0);
      }
      return fromArrayLike(that, obj);
    }
    if (obj.type === "Buffer" && isArray(obj.data)) {
      return fromArrayLike(that, obj.data);
    }
  }
  throw new TypeError("First argument must be a string, Buffer, ArrayBuffer, Array, or array-like object.");
}
function checked(length) {
  if (length >= kMaxLength()) {
    throw new RangeError("Attempt to allocate Buffer larger than maximum size: 0x" + kMaxLength().toString(16) + " bytes");
  }
  return length | 0;
}
function internalIsBuffer(b) {
  return !!(b != null && b._isBuffer);
}
function byteLength(string, encoding) {
  if (internalIsBuffer(string)) {
    return string.length;
  }
  if (typeof ArrayBuffer !== "undefined" && typeof ArrayBuffer.isView === "function" && (ArrayBuffer.isView(string) || string instanceof ArrayBuffer)) {
    return string.byteLength;
  }
  if (typeof string !== "string") {
    string = "" + string;
  }
  var len = string.length;
  if (len === 0)
    return 0;
  var loweredCase = false;
  for (; ; ) {
    switch (encoding) {
      case "ascii":
      case "latin1":
      case "binary":
        return len;
      case "utf8":
      case "utf-8":
      case void 0:
        return utf8ToBytes(string).length;
      case "ucs2":
      case "ucs-2":
      case "utf16le":
      case "utf-16le":
        return len * 2;
      case "hex":
        return len >>> 1;
      case "base64":
        return base64ToBytes(string).length;
      default:
        if (loweredCase)
          return utf8ToBytes(string).length;
        encoding = ("" + encoding).toLowerCase();
        loweredCase = true;
    }
  }
}
function slowToString(encoding, start, end) {
  var loweredCase = false;
  if (start === void 0 || start < 0) {
    start = 0;
  }
  if (start > this.length) {
    return "";
  }
  if (end === void 0 || end > this.length) {
    end = this.length;
  }
  if (end <= 0) {
    return "";
  }
  end >>>= 0;
  start >>>= 0;
  if (end <= start) {
    return "";
  }
  if (!encoding)
    encoding = "utf8";
  while (true) {
    switch (encoding) {
      case "hex":
        return hexSlice(this, start, end);
      case "utf8":
      case "utf-8":
        return utf8Slice(this, start, end);
      case "ascii":
        return asciiSlice(this, start, end);
      case "latin1":
      case "binary":
        return latin1Slice(this, start, end);
      case "base64":
        return base64Slice(this, start, end);
      case "ucs2":
      case "ucs-2":
      case "utf16le":
      case "utf-16le":
        return utf16leSlice(this, start, end);
      default:
        if (loweredCase)
          throw new TypeError("Unknown encoding: " + encoding);
        encoding = (encoding + "").toLowerCase();
        loweredCase = true;
    }
  }
}
function swap(b, n, m) {
  var i = b[n];
  b[n] = b[m];
  b[m] = i;
}
function bidirectionalIndexOf(buffer, val, byteOffset, encoding, dir) {
  if (buffer.length === 0)
    return -1;
  if (typeof byteOffset === "string") {
    encoding = byteOffset;
    byteOffset = 0;
  } else if (byteOffset > 2147483647) {
    byteOffset = 2147483647;
  } else if (byteOffset < -2147483648) {
    byteOffset = -2147483648;
  }
  byteOffset = +byteOffset;
  if (isNaN(byteOffset)) {
    byteOffset = dir ? 0 : buffer.length - 1;
  }
  if (byteOffset < 0)
    byteOffset = buffer.length + byteOffset;
  if (byteOffset >= buffer.length) {
    if (dir)
      return -1;
    else
      byteOffset = buffer.length - 1;
  } else if (byteOffset < 0) {
    if (dir)
      byteOffset = 0;
    else
      return -1;
  }
  if (typeof val === "string") {
    val = Buffer2.from(val, encoding);
  }
  if (internalIsBuffer(val)) {
    if (val.length === 0) {
      return -1;
    }
    return arrayIndexOf(buffer, val, byteOffset, encoding, dir);
  } else if (typeof val === "number") {
    val = val & 255;
    if (Buffer2.TYPED_ARRAY_SUPPORT && typeof Uint8Array.prototype.indexOf === "function") {
      if (dir) {
        return Uint8Array.prototype.indexOf.call(buffer, val, byteOffset);
      } else {
        return Uint8Array.prototype.lastIndexOf.call(buffer, val, byteOffset);
      }
    }
    return arrayIndexOf(buffer, [val], byteOffset, encoding, dir);
  }
  throw new TypeError("val must be string, number or Buffer");
}
function arrayIndexOf(arr, val, byteOffset, encoding, dir) {
  var indexSize = 1;
  var arrLength = arr.length;
  var valLength = val.length;
  if (encoding !== void 0) {
    encoding = String(encoding).toLowerCase();
    if (encoding === "ucs2" || encoding === "ucs-2" || encoding === "utf16le" || encoding === "utf-16le") {
      if (arr.length < 2 || val.length < 2) {
        return -1;
      }
      indexSize = 2;
      arrLength /= 2;
      valLength /= 2;
      byteOffset /= 2;
    }
  }
  function read2(buf, i2) {
    if (indexSize === 1) {
      return buf[i2];
    } else {
      return buf.readUInt16BE(i2 * indexSize);
    }
  }
  var i;
  if (dir) {
    var foundIndex = -1;
    for (i = byteOffset; i < arrLength; i++) {
      if (read2(arr, i) === read2(val, foundIndex === -1 ? 0 : i - foundIndex)) {
        if (foundIndex === -1)
          foundIndex = i;
        if (i - foundIndex + 1 === valLength)
          return foundIndex * indexSize;
      } else {
        if (foundIndex !== -1)
          i -= i - foundIndex;
        foundIndex = -1;
      }
    }
  } else {
    if (byteOffset + valLength > arrLength)
      byteOffset = arrLength - valLength;
    for (i = byteOffset; i >= 0; i--) {
      var found = true;
      for (var j = 0; j < valLength; j++) {
        if (read2(arr, i + j) !== read2(val, j)) {
          found = false;
          break;
        }
      }
      if (found)
        return i;
    }
  }
  return -1;
}
function hexWrite(buf, string, offset, length) {
  offset = Number(offset) || 0;
  var remaining = buf.length - offset;
  if (!length) {
    length = remaining;
  } else {
    length = Number(length);
    if (length > remaining) {
      length = remaining;
    }
  }
  var strLen = string.length;
  if (strLen % 2 !== 0)
    throw new TypeError("Invalid hex string");
  if (length > strLen / 2) {
    length = strLen / 2;
  }
  for (var i = 0; i < length; ++i) {
    var parsed = parseInt(string.substr(i * 2, 2), 16);
    if (isNaN(parsed))
      return i;
    buf[offset + i] = parsed;
  }
  return i;
}
function utf8Write(buf, string, offset, length) {
  return blitBuffer(utf8ToBytes(string, buf.length - offset), buf, offset, length);
}
function asciiWrite(buf, string, offset, length) {
  return blitBuffer(asciiToBytes(string), buf, offset, length);
}
function latin1Write(buf, string, offset, length) {
  return asciiWrite(buf, string, offset, length);
}
function base64Write(buf, string, offset, length) {
  return blitBuffer(base64ToBytes(string), buf, offset, length);
}
function ucs2Write(buf, string, offset, length) {
  return blitBuffer(utf16leToBytes(string, buf.length - offset), buf, offset, length);
}
function base64Slice(buf, start, end) {
  if (start === 0 && end === buf.length) {
    return fromByteArray(buf);
  } else {
    return fromByteArray(buf.slice(start, end));
  }
}
function utf8Slice(buf, start, end) {
  end = Math.min(buf.length, end);
  var res = [];
  var i = start;
  while (i < end) {
    var firstByte = buf[i];
    var codePoint = null;
    var bytesPerSequence = firstByte > 239 ? 4 : firstByte > 223 ? 3 : firstByte > 191 ? 2 : 1;
    if (i + bytesPerSequence <= end) {
      var secondByte, thirdByte, fourthByte, tempCodePoint;
      switch (bytesPerSequence) {
        case 1:
          if (firstByte < 128) {
            codePoint = firstByte;
          }
          break;
        case 2:
          secondByte = buf[i + 1];
          if ((secondByte & 192) === 128) {
            tempCodePoint = (firstByte & 31) << 6 | secondByte & 63;
            if (tempCodePoint > 127) {
              codePoint = tempCodePoint;
            }
          }
          break;
        case 3:
          secondByte = buf[i + 1];
          thirdByte = buf[i + 2];
          if ((secondByte & 192) === 128 && (thirdByte & 192) === 128) {
            tempCodePoint = (firstByte & 15) << 12 | (secondByte & 63) << 6 | thirdByte & 63;
            if (tempCodePoint > 2047 && (tempCodePoint < 55296 || tempCodePoint > 57343)) {
              codePoint = tempCodePoint;
            }
          }
          break;
        case 4:
          secondByte = buf[i + 1];
          thirdByte = buf[i + 2];
          fourthByte = buf[i + 3];
          if ((secondByte & 192) === 128 && (thirdByte & 192) === 128 && (fourthByte & 192) === 128) {
            tempCodePoint = (firstByte & 15) << 18 | (secondByte & 63) << 12 | (thirdByte & 63) << 6 | fourthByte & 63;
            if (tempCodePoint > 65535 && tempCodePoint < 1114112) {
              codePoint = tempCodePoint;
            }
          }
      }
    }
    if (codePoint === null) {
      codePoint = 65533;
      bytesPerSequence = 1;
    } else if (codePoint > 65535) {
      codePoint -= 65536;
      res.push(codePoint >>> 10 & 1023 | 55296);
      codePoint = 56320 | codePoint & 1023;
    }
    res.push(codePoint);
    i += bytesPerSequence;
  }
  return decodeCodePointsArray(res);
}
function decodeCodePointsArray(codePoints) {
  var len = codePoints.length;
  if (len <= MAX_ARGUMENTS_LENGTH) {
    return String.fromCharCode.apply(String, codePoints);
  }
  var res = "";
  var i = 0;
  while (i < len) {
    res += String.fromCharCode.apply(
      String,
      codePoints.slice(i, i += MAX_ARGUMENTS_LENGTH)
    );
  }
  return res;
}
function asciiSlice(buf, start, end) {
  var ret = "";
  end = Math.min(buf.length, end);
  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i] & 127);
  }
  return ret;
}
function latin1Slice(buf, start, end) {
  var ret = "";
  end = Math.min(buf.length, end);
  for (var i = start; i < end; ++i) {
    ret += String.fromCharCode(buf[i]);
  }
  return ret;
}
function hexSlice(buf, start, end) {
  var len = buf.length;
  if (!start || start < 0)
    start = 0;
  if (!end || end < 0 || end > len)
    end = len;
  var out = "";
  for (var i = start; i < end; ++i) {
    out += toHex(buf[i]);
  }
  return out;
}
function utf16leSlice(buf, start, end) {
  var bytes = buf.slice(start, end);
  var res = "";
  for (var i = 0; i < bytes.length; i += 2) {
    res += String.fromCharCode(bytes[i] + bytes[i + 1] * 256);
  }
  return res;
}
function checkOffset(offset, ext, length) {
  if (offset % 1 !== 0 || offset < 0)
    throw new RangeError("offset is not uint");
  if (offset + ext > length)
    throw new RangeError("Trying to access beyond buffer length");
}
function checkInt(buf, value, offset, ext, max, min) {
  if (!internalIsBuffer(buf))
    throw new TypeError('"buffer" argument must be a Buffer instance');
  if (value > max || value < min)
    throw new RangeError('"value" argument is out of bounds');
  if (offset + ext > buf.length)
    throw new RangeError("Index out of range");
}
function objectWriteUInt16(buf, value, offset, littleEndian) {
  if (value < 0)
    value = 65535 + value + 1;
  for (var i = 0, j = Math.min(buf.length - offset, 2); i < j; ++i) {
    buf[offset + i] = (value & 255 << 8 * (littleEndian ? i : 1 - i)) >>> (littleEndian ? i : 1 - i) * 8;
  }
}
function objectWriteUInt32(buf, value, offset, littleEndian) {
  if (value < 0)
    value = 4294967295 + value + 1;
  for (var i = 0, j = Math.min(buf.length - offset, 4); i < j; ++i) {
    buf[offset + i] = value >>> (littleEndian ? i : 3 - i) * 8 & 255;
  }
}
function checkIEEE754(buf, value, offset, ext, max, min) {
  if (offset + ext > buf.length)
    throw new RangeError("Index out of range");
  if (offset < 0)
    throw new RangeError("Index out of range");
}
function writeFloat(buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 4);
  }
  write(buf, value, offset, littleEndian, 23, 4);
  return offset + 4;
}
function writeDouble(buf, value, offset, littleEndian, noAssert) {
  if (!noAssert) {
    checkIEEE754(buf, value, offset, 8);
  }
  write(buf, value, offset, littleEndian, 52, 8);
  return offset + 8;
}
function base64clean(str) {
  str = stringtrim(str).replace(INVALID_BASE64_RE, "");
  if (str.length < 2)
    return "";
  while (str.length % 4 !== 0) {
    str = str + "=";
  }
  return str;
}
function stringtrim(str) {
  if (str.trim)
    return str.trim();
  return str.replace(/^\s+|\s+$/g, "");
}
function toHex(n) {
  if (n < 16)
    return "0" + n.toString(16);
  return n.toString(16);
}
function utf8ToBytes(string, units) {
  units = units || Infinity;
  var codePoint;
  var length = string.length;
  var leadSurrogate = null;
  var bytes = [];
  for (var i = 0; i < length; ++i) {
    codePoint = string.charCodeAt(i);
    if (codePoint > 55295 && codePoint < 57344) {
      if (!leadSurrogate) {
        if (codePoint > 56319) {
          if ((units -= 3) > -1)
            bytes.push(239, 191, 189);
          continue;
        } else if (i + 1 === length) {
          if ((units -= 3) > -1)
            bytes.push(239, 191, 189);
          continue;
        }
        leadSurrogate = codePoint;
        continue;
      }
      if (codePoint < 56320) {
        if ((units -= 3) > -1)
          bytes.push(239, 191, 189);
        leadSurrogate = codePoint;
        continue;
      }
      codePoint = (leadSurrogate - 55296 << 10 | codePoint - 56320) + 65536;
    } else if (leadSurrogate) {
      if ((units -= 3) > -1)
        bytes.push(239, 191, 189);
    }
    leadSurrogate = null;
    if (codePoint < 128) {
      if ((units -= 1) < 0)
        break;
      bytes.push(codePoint);
    } else if (codePoint < 2048) {
      if ((units -= 2) < 0)
        break;
      bytes.push(
        codePoint >> 6 | 192,
        codePoint & 63 | 128
      );
    } else if (codePoint < 65536) {
      if ((units -= 3) < 0)
        break;
      bytes.push(
        codePoint >> 12 | 224,
        codePoint >> 6 & 63 | 128,
        codePoint & 63 | 128
      );
    } else if (codePoint < 1114112) {
      if ((units -= 4) < 0)
        break;
      bytes.push(
        codePoint >> 18 | 240,
        codePoint >> 12 & 63 | 128,
        codePoint >> 6 & 63 | 128,
        codePoint & 63 | 128
      );
    } else {
      throw new Error("Invalid code point");
    }
  }
  return bytes;
}
function asciiToBytes(str) {
  var byteArray = [];
  for (var i = 0; i < str.length; ++i) {
    byteArray.push(str.charCodeAt(i) & 255);
  }
  return byteArray;
}
function utf16leToBytes(str, units) {
  var c, hi, lo;
  var byteArray = [];
  for (var i = 0; i < str.length; ++i) {
    if ((units -= 2) < 0)
      break;
    c = str.charCodeAt(i);
    hi = c >> 8;
    lo = c % 256;
    byteArray.push(lo);
    byteArray.push(hi);
  }
  return byteArray;
}
function base64ToBytes(str) {
  return toByteArray(base64clean(str));
}
function blitBuffer(src, dst, offset, length) {
  for (var i = 0; i < length; ++i) {
    if (i + offset >= dst.length || i >= src.length)
      break;
    dst[i + offset] = src[i];
  }
  return i;
}
function isnan(val) {
  return val !== val;
}
function isBuffer(obj) {
  return obj != null && (!!obj._isBuffer || isFastBuffer(obj) || isSlowBuffer(obj));
}
function isFastBuffer(obj) {
  return !!obj.constructor && typeof obj.constructor.isBuffer === "function" && obj.constructor.isBuffer(obj);
}
function isSlowBuffer(obj) {
  return typeof obj.readFloatLE === "function" && typeof obj.slice === "function" && isFastBuffer(obj.slice(0, 0));
}
var global$1, lookup, revLookup, Arr, inited, toString, isArray, INSPECT_MAX_BYTES, MAX_ARGUMENTS_LENGTH, INVALID_BASE64_RE, CsvError, is_object, normalize_columns_array, ResizeableBuffer, np, cr$1, nl$1, space, tab, init_state, underscore, normalize_options, isRecordEmpty, cr, nl, boms, transform, parse;
var init_sync = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/dist/esm/sync.js"() {
    global$1 = typeof window !== "undefined" ? window : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {};
    lookup = [];
    revLookup = [];
    Arr = typeof Uint8Array !== "undefined" ? Uint8Array : Array;
    inited = false;
    toString = {}.toString;
    isArray = Array.isArray || function(arr) {
      return toString.call(arr) == "[object Array]";
    };
    INSPECT_MAX_BYTES = 50;
    Buffer2.TYPED_ARRAY_SUPPORT = global$1.TYPED_ARRAY_SUPPORT !== void 0 ? global$1.TYPED_ARRAY_SUPPORT : true;
    kMaxLength();
    Buffer2.poolSize = 8192;
    Buffer2._augment = function(arr) {
      arr.__proto__ = Buffer2.prototype;
      return arr;
    };
    Buffer2.from = function(value, encodingOrOffset, length) {
      return from(null, value, encodingOrOffset, length);
    };
    if (Buffer2.TYPED_ARRAY_SUPPORT) {
      Buffer2.prototype.__proto__ = Uint8Array.prototype;
      Buffer2.__proto__ = Uint8Array;
      if (typeof Symbol !== "undefined" && Symbol.species && Buffer2[Symbol.species] === Buffer2)
        ;
    }
    Buffer2.alloc = function(size, fill2, encoding) {
      return alloc(null, size, fill2, encoding);
    };
    Buffer2.allocUnsafe = function(size) {
      return allocUnsafe(null, size);
    };
    Buffer2.allocUnsafeSlow = function(size) {
      return allocUnsafe(null, size);
    };
    Buffer2.isBuffer = isBuffer;
    Buffer2.compare = function compare(a, b) {
      if (!internalIsBuffer(a) || !internalIsBuffer(b)) {
        throw new TypeError("Arguments must be Buffers");
      }
      if (a === b)
        return 0;
      var x = a.length;
      var y = b.length;
      for (var i = 0, len = Math.min(x, y); i < len; ++i) {
        if (a[i] !== b[i]) {
          x = a[i];
          y = b[i];
          break;
        }
      }
      if (x < y)
        return -1;
      if (y < x)
        return 1;
      return 0;
    };
    Buffer2.isEncoding = function isEncoding(encoding) {
      switch (String(encoding).toLowerCase()) {
        case "hex":
        case "utf8":
        case "utf-8":
        case "ascii":
        case "latin1":
        case "binary":
        case "base64":
        case "ucs2":
        case "ucs-2":
        case "utf16le":
        case "utf-16le":
          return true;
        default:
          return false;
      }
    };
    Buffer2.concat = function concat(list, length) {
      if (!isArray(list)) {
        throw new TypeError('"list" argument must be an Array of Buffers');
      }
      if (list.length === 0) {
        return Buffer2.alloc(0);
      }
      var i;
      if (length === void 0) {
        length = 0;
        for (i = 0; i < list.length; ++i) {
          length += list[i].length;
        }
      }
      var buffer = Buffer2.allocUnsafe(length);
      var pos = 0;
      for (i = 0; i < list.length; ++i) {
        var buf = list[i];
        if (!internalIsBuffer(buf)) {
          throw new TypeError('"list" argument must be an Array of Buffers');
        }
        buf.copy(buffer, pos);
        pos += buf.length;
      }
      return buffer;
    };
    Buffer2.byteLength = byteLength;
    Buffer2.prototype._isBuffer = true;
    Buffer2.prototype.swap16 = function swap16() {
      var len = this.length;
      if (len % 2 !== 0) {
        throw new RangeError("Buffer size must be a multiple of 16-bits");
      }
      for (var i = 0; i < len; i += 2) {
        swap(this, i, i + 1);
      }
      return this;
    };
    Buffer2.prototype.swap32 = function swap32() {
      var len = this.length;
      if (len % 4 !== 0) {
        throw new RangeError("Buffer size must be a multiple of 32-bits");
      }
      for (var i = 0; i < len; i += 4) {
        swap(this, i, i + 3);
        swap(this, i + 1, i + 2);
      }
      return this;
    };
    Buffer2.prototype.swap64 = function swap64() {
      var len = this.length;
      if (len % 8 !== 0) {
        throw new RangeError("Buffer size must be a multiple of 64-bits");
      }
      for (var i = 0; i < len; i += 8) {
        swap(this, i, i + 7);
        swap(this, i + 1, i + 6);
        swap(this, i + 2, i + 5);
        swap(this, i + 3, i + 4);
      }
      return this;
    };
    Buffer2.prototype.toString = function toString2() {
      var length = this.length | 0;
      if (length === 0)
        return "";
      if (arguments.length === 0)
        return utf8Slice(this, 0, length);
      return slowToString.apply(this, arguments);
    };
    Buffer2.prototype.equals = function equals(b) {
      if (!internalIsBuffer(b))
        throw new TypeError("Argument must be a Buffer");
      if (this === b)
        return true;
      return Buffer2.compare(this, b) === 0;
    };
    Buffer2.prototype.inspect = function inspect() {
      var str = "";
      var max = INSPECT_MAX_BYTES;
      if (this.length > 0) {
        str = this.toString("hex", 0, max).match(/.{2}/g).join(" ");
        if (this.length > max)
          str += " ... ";
      }
      return "<Buffer " + str + ">";
    };
    Buffer2.prototype.compare = function compare2(target, start, end, thisStart, thisEnd) {
      if (!internalIsBuffer(target)) {
        throw new TypeError("Argument must be a Buffer");
      }
      if (start === void 0) {
        start = 0;
      }
      if (end === void 0) {
        end = target ? target.length : 0;
      }
      if (thisStart === void 0) {
        thisStart = 0;
      }
      if (thisEnd === void 0) {
        thisEnd = this.length;
      }
      if (start < 0 || end > target.length || thisStart < 0 || thisEnd > this.length) {
        throw new RangeError("out of range index");
      }
      if (thisStart >= thisEnd && start >= end) {
        return 0;
      }
      if (thisStart >= thisEnd) {
        return -1;
      }
      if (start >= end) {
        return 1;
      }
      start >>>= 0;
      end >>>= 0;
      thisStart >>>= 0;
      thisEnd >>>= 0;
      if (this === target)
        return 0;
      var x = thisEnd - thisStart;
      var y = end - start;
      var len = Math.min(x, y);
      var thisCopy = this.slice(thisStart, thisEnd);
      var targetCopy = target.slice(start, end);
      for (var i = 0; i < len; ++i) {
        if (thisCopy[i] !== targetCopy[i]) {
          x = thisCopy[i];
          y = targetCopy[i];
          break;
        }
      }
      if (x < y)
        return -1;
      if (y < x)
        return 1;
      return 0;
    };
    Buffer2.prototype.includes = function includes(val, byteOffset, encoding) {
      return this.indexOf(val, byteOffset, encoding) !== -1;
    };
    Buffer2.prototype.indexOf = function indexOf(val, byteOffset, encoding) {
      return bidirectionalIndexOf(this, val, byteOffset, encoding, true);
    };
    Buffer2.prototype.lastIndexOf = function lastIndexOf(val, byteOffset, encoding) {
      return bidirectionalIndexOf(this, val, byteOffset, encoding, false);
    };
    Buffer2.prototype.write = function write2(string, offset, length, encoding) {
      if (offset === void 0) {
        encoding = "utf8";
        length = this.length;
        offset = 0;
      } else if (length === void 0 && typeof offset === "string") {
        encoding = offset;
        length = this.length;
        offset = 0;
      } else if (isFinite(offset)) {
        offset = offset | 0;
        if (isFinite(length)) {
          length = length | 0;
          if (encoding === void 0)
            encoding = "utf8";
        } else {
          encoding = length;
          length = void 0;
        }
      } else {
        throw new Error(
          "Buffer.write(string, encoding, offset[, length]) is no longer supported"
        );
      }
      var remaining = this.length - offset;
      if (length === void 0 || length > remaining)
        length = remaining;
      if (string.length > 0 && (length < 0 || offset < 0) || offset > this.length) {
        throw new RangeError("Attempt to write outside buffer bounds");
      }
      if (!encoding)
        encoding = "utf8";
      var loweredCase = false;
      for (; ; ) {
        switch (encoding) {
          case "hex":
            return hexWrite(this, string, offset, length);
          case "utf8":
          case "utf-8":
            return utf8Write(this, string, offset, length);
          case "ascii":
            return asciiWrite(this, string, offset, length);
          case "latin1":
          case "binary":
            return latin1Write(this, string, offset, length);
          case "base64":
            return base64Write(this, string, offset, length);
          case "ucs2":
          case "ucs-2":
          case "utf16le":
          case "utf-16le":
            return ucs2Write(this, string, offset, length);
          default:
            if (loweredCase)
              throw new TypeError("Unknown encoding: " + encoding);
            encoding = ("" + encoding).toLowerCase();
            loweredCase = true;
        }
      }
    };
    Buffer2.prototype.toJSON = function toJSON() {
      return {
        type: "Buffer",
        data: Array.prototype.slice.call(this._arr || this, 0)
      };
    };
    MAX_ARGUMENTS_LENGTH = 4096;
    Buffer2.prototype.slice = function slice(start, end) {
      var len = this.length;
      start = ~~start;
      end = end === void 0 ? len : ~~end;
      if (start < 0) {
        start += len;
        if (start < 0)
          start = 0;
      } else if (start > len) {
        start = len;
      }
      if (end < 0) {
        end += len;
        if (end < 0)
          end = 0;
      } else if (end > len) {
        end = len;
      }
      if (end < start)
        end = start;
      var newBuf;
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        newBuf = this.subarray(start, end);
        newBuf.__proto__ = Buffer2.prototype;
      } else {
        var sliceLen = end - start;
        newBuf = new Buffer2(sliceLen, void 0);
        for (var i = 0; i < sliceLen; ++i) {
          newBuf[i] = this[i + start];
        }
      }
      return newBuf;
    };
    Buffer2.prototype.readUIntLE = function readUIntLE(offset, byteLength2, noAssert) {
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert)
        checkOffset(offset, byteLength2, this.length);
      var val = this[offset];
      var mul = 1;
      var i = 0;
      while (++i < byteLength2 && (mul *= 256)) {
        val += this[offset + i] * mul;
      }
      return val;
    };
    Buffer2.prototype.readUIntBE = function readUIntBE(offset, byteLength2, noAssert) {
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert) {
        checkOffset(offset, byteLength2, this.length);
      }
      var val = this[offset + --byteLength2];
      var mul = 1;
      while (byteLength2 > 0 && (mul *= 256)) {
        val += this[offset + --byteLength2] * mul;
      }
      return val;
    };
    Buffer2.prototype.readUInt8 = function readUInt8(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 1, this.length);
      return this[offset];
    };
    Buffer2.prototype.readUInt16LE = function readUInt16LE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 2, this.length);
      return this[offset] | this[offset + 1] << 8;
    };
    Buffer2.prototype.readUInt16BE = function readUInt16BE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 2, this.length);
      return this[offset] << 8 | this[offset + 1];
    };
    Buffer2.prototype.readUInt32LE = function readUInt32LE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return (this[offset] | this[offset + 1] << 8 | this[offset + 2] << 16) + this[offset + 3] * 16777216;
    };
    Buffer2.prototype.readUInt32BE = function readUInt32BE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return this[offset] * 16777216 + (this[offset + 1] << 16 | this[offset + 2] << 8 | this[offset + 3]);
    };
    Buffer2.prototype.readIntLE = function readIntLE(offset, byteLength2, noAssert) {
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert)
        checkOffset(offset, byteLength2, this.length);
      var val = this[offset];
      var mul = 1;
      var i = 0;
      while (++i < byteLength2 && (mul *= 256)) {
        val += this[offset + i] * mul;
      }
      mul *= 128;
      if (val >= mul)
        val -= Math.pow(2, 8 * byteLength2);
      return val;
    };
    Buffer2.prototype.readIntBE = function readIntBE(offset, byteLength2, noAssert) {
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert)
        checkOffset(offset, byteLength2, this.length);
      var i = byteLength2;
      var mul = 1;
      var val = this[offset + --i];
      while (i > 0 && (mul *= 256)) {
        val += this[offset + --i] * mul;
      }
      mul *= 128;
      if (val >= mul)
        val -= Math.pow(2, 8 * byteLength2);
      return val;
    };
    Buffer2.prototype.readInt8 = function readInt8(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 1, this.length);
      if (!(this[offset] & 128))
        return this[offset];
      return (255 - this[offset] + 1) * -1;
    };
    Buffer2.prototype.readInt16LE = function readInt16LE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 2, this.length);
      var val = this[offset] | this[offset + 1] << 8;
      return val & 32768 ? val | 4294901760 : val;
    };
    Buffer2.prototype.readInt16BE = function readInt16BE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 2, this.length);
      var val = this[offset + 1] | this[offset] << 8;
      return val & 32768 ? val | 4294901760 : val;
    };
    Buffer2.prototype.readInt32LE = function readInt32LE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return this[offset] | this[offset + 1] << 8 | this[offset + 2] << 16 | this[offset + 3] << 24;
    };
    Buffer2.prototype.readInt32BE = function readInt32BE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return this[offset] << 24 | this[offset + 1] << 16 | this[offset + 2] << 8 | this[offset + 3];
    };
    Buffer2.prototype.readFloatLE = function readFloatLE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return read(this, offset, true, 23, 4);
    };
    Buffer2.prototype.readFloatBE = function readFloatBE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 4, this.length);
      return read(this, offset, false, 23, 4);
    };
    Buffer2.prototype.readDoubleLE = function readDoubleLE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 8, this.length);
      return read(this, offset, true, 52, 8);
    };
    Buffer2.prototype.readDoubleBE = function readDoubleBE(offset, noAssert) {
      if (!noAssert)
        checkOffset(offset, 8, this.length);
      return read(this, offset, false, 52, 8);
    };
    Buffer2.prototype.writeUIntLE = function writeUIntLE(value, offset, byteLength2, noAssert) {
      value = +value;
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert) {
        var maxBytes = Math.pow(2, 8 * byteLength2) - 1;
        checkInt(this, value, offset, byteLength2, maxBytes, 0);
      }
      var mul = 1;
      var i = 0;
      this[offset] = value & 255;
      while (++i < byteLength2 && (mul *= 256)) {
        this[offset + i] = value / mul & 255;
      }
      return offset + byteLength2;
    };
    Buffer2.prototype.writeUIntBE = function writeUIntBE(value, offset, byteLength2, noAssert) {
      value = +value;
      offset = offset | 0;
      byteLength2 = byteLength2 | 0;
      if (!noAssert) {
        var maxBytes = Math.pow(2, 8 * byteLength2) - 1;
        checkInt(this, value, offset, byteLength2, maxBytes, 0);
      }
      var i = byteLength2 - 1;
      var mul = 1;
      this[offset + i] = value & 255;
      while (--i >= 0 && (mul *= 256)) {
        this[offset + i] = value / mul & 255;
      }
      return offset + byteLength2;
    };
    Buffer2.prototype.writeUInt8 = function writeUInt8(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 1, 255, 0);
      if (!Buffer2.TYPED_ARRAY_SUPPORT)
        value = Math.floor(value);
      this[offset] = value & 255;
      return offset + 1;
    };
    Buffer2.prototype.writeUInt16LE = function writeUInt16LE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 2, 65535, 0);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value & 255;
        this[offset + 1] = value >>> 8;
      } else {
        objectWriteUInt16(this, value, offset, true);
      }
      return offset + 2;
    };
    Buffer2.prototype.writeUInt16BE = function writeUInt16BE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 2, 65535, 0);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value >>> 8;
        this[offset + 1] = value & 255;
      } else {
        objectWriteUInt16(this, value, offset, false);
      }
      return offset + 2;
    };
    Buffer2.prototype.writeUInt32LE = function writeUInt32LE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 4, 4294967295, 0);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset + 3] = value >>> 24;
        this[offset + 2] = value >>> 16;
        this[offset + 1] = value >>> 8;
        this[offset] = value & 255;
      } else {
        objectWriteUInt32(this, value, offset, true);
      }
      return offset + 4;
    };
    Buffer2.prototype.writeUInt32BE = function writeUInt32BE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 4, 4294967295, 0);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value >>> 24;
        this[offset + 1] = value >>> 16;
        this[offset + 2] = value >>> 8;
        this[offset + 3] = value & 255;
      } else {
        objectWriteUInt32(this, value, offset, false);
      }
      return offset + 4;
    };
    Buffer2.prototype.writeIntLE = function writeIntLE(value, offset, byteLength2, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert) {
        var limit = Math.pow(2, 8 * byteLength2 - 1);
        checkInt(this, value, offset, byteLength2, limit - 1, -limit);
      }
      var i = 0;
      var mul = 1;
      var sub = 0;
      this[offset] = value & 255;
      while (++i < byteLength2 && (mul *= 256)) {
        if (value < 0 && sub === 0 && this[offset + i - 1] !== 0) {
          sub = 1;
        }
        this[offset + i] = (value / mul >> 0) - sub & 255;
      }
      return offset + byteLength2;
    };
    Buffer2.prototype.writeIntBE = function writeIntBE(value, offset, byteLength2, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert) {
        var limit = Math.pow(2, 8 * byteLength2 - 1);
        checkInt(this, value, offset, byteLength2, limit - 1, -limit);
      }
      var i = byteLength2 - 1;
      var mul = 1;
      var sub = 0;
      this[offset + i] = value & 255;
      while (--i >= 0 && (mul *= 256)) {
        if (value < 0 && sub === 0 && this[offset + i + 1] !== 0) {
          sub = 1;
        }
        this[offset + i] = (value / mul >> 0) - sub & 255;
      }
      return offset + byteLength2;
    };
    Buffer2.prototype.writeInt8 = function writeInt8(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 1, 127, -128);
      if (!Buffer2.TYPED_ARRAY_SUPPORT)
        value = Math.floor(value);
      if (value < 0)
        value = 255 + value + 1;
      this[offset] = value & 255;
      return offset + 1;
    };
    Buffer2.prototype.writeInt16LE = function writeInt16LE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 2, 32767, -32768);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value & 255;
        this[offset + 1] = value >>> 8;
      } else {
        objectWriteUInt16(this, value, offset, true);
      }
      return offset + 2;
    };
    Buffer2.prototype.writeInt16BE = function writeInt16BE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 2, 32767, -32768);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value >>> 8;
        this[offset + 1] = value & 255;
      } else {
        objectWriteUInt16(this, value, offset, false);
      }
      return offset + 2;
    };
    Buffer2.prototype.writeInt32LE = function writeInt32LE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 4, 2147483647, -2147483648);
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value & 255;
        this[offset + 1] = value >>> 8;
        this[offset + 2] = value >>> 16;
        this[offset + 3] = value >>> 24;
      } else {
        objectWriteUInt32(this, value, offset, true);
      }
      return offset + 4;
    };
    Buffer2.prototype.writeInt32BE = function writeInt32BE(value, offset, noAssert) {
      value = +value;
      offset = offset | 0;
      if (!noAssert)
        checkInt(this, value, offset, 4, 2147483647, -2147483648);
      if (value < 0)
        value = 4294967295 + value + 1;
      if (Buffer2.TYPED_ARRAY_SUPPORT) {
        this[offset] = value >>> 24;
        this[offset + 1] = value >>> 16;
        this[offset + 2] = value >>> 8;
        this[offset + 3] = value & 255;
      } else {
        objectWriteUInt32(this, value, offset, false);
      }
      return offset + 4;
    };
    Buffer2.prototype.writeFloatLE = function writeFloatLE(value, offset, noAssert) {
      return writeFloat(this, value, offset, true, noAssert);
    };
    Buffer2.prototype.writeFloatBE = function writeFloatBE(value, offset, noAssert) {
      return writeFloat(this, value, offset, false, noAssert);
    };
    Buffer2.prototype.writeDoubleLE = function writeDoubleLE(value, offset, noAssert) {
      return writeDouble(this, value, offset, true, noAssert);
    };
    Buffer2.prototype.writeDoubleBE = function writeDoubleBE(value, offset, noAssert) {
      return writeDouble(this, value, offset, false, noAssert);
    };
    Buffer2.prototype.copy = function copy(target, targetStart, start, end) {
      if (!start)
        start = 0;
      if (!end && end !== 0)
        end = this.length;
      if (targetStart >= target.length)
        targetStart = target.length;
      if (!targetStart)
        targetStart = 0;
      if (end > 0 && end < start)
        end = start;
      if (end === start)
        return 0;
      if (target.length === 0 || this.length === 0)
        return 0;
      if (targetStart < 0) {
        throw new RangeError("targetStart out of bounds");
      }
      if (start < 0 || start >= this.length)
        throw new RangeError("sourceStart out of bounds");
      if (end < 0)
        throw new RangeError("sourceEnd out of bounds");
      if (end > this.length)
        end = this.length;
      if (target.length - targetStart < end - start) {
        end = target.length - targetStart + start;
      }
      var len = end - start;
      var i;
      if (this === target && start < targetStart && targetStart < end) {
        for (i = len - 1; i >= 0; --i) {
          target[i + targetStart] = this[i + start];
        }
      } else if (len < 1e3 || !Buffer2.TYPED_ARRAY_SUPPORT) {
        for (i = 0; i < len; ++i) {
          target[i + targetStart] = this[i + start];
        }
      } else {
        Uint8Array.prototype.set.call(
          target,
          this.subarray(start, start + len),
          targetStart
        );
      }
      return len;
    };
    Buffer2.prototype.fill = function fill(val, start, end, encoding) {
      if (typeof val === "string") {
        if (typeof start === "string") {
          encoding = start;
          start = 0;
          end = this.length;
        } else if (typeof end === "string") {
          encoding = end;
          end = this.length;
        }
        if (val.length === 1) {
          var code = val.charCodeAt(0);
          if (code < 256) {
            val = code;
          }
        }
        if (encoding !== void 0 && typeof encoding !== "string") {
          throw new TypeError("encoding must be a string");
        }
        if (typeof encoding === "string" && !Buffer2.isEncoding(encoding)) {
          throw new TypeError("Unknown encoding: " + encoding);
        }
      } else if (typeof val === "number") {
        val = val & 255;
      }
      if (start < 0 || this.length < start || this.length < end) {
        throw new RangeError("Out of range index");
      }
      if (end <= start) {
        return this;
      }
      start = start >>> 0;
      end = end === void 0 ? this.length : end >>> 0;
      if (!val)
        val = 0;
      var i;
      if (typeof val === "number") {
        for (i = start; i < end; ++i) {
          this[i] = val;
        }
      } else {
        var bytes = internalIsBuffer(val) ? val : utf8ToBytes(new Buffer2(val, encoding).toString());
        var len = bytes.length;
        for (i = 0; i < end - start; ++i) {
          this[i + start] = bytes[i % len];
        }
      }
      return this;
    };
    INVALID_BASE64_RE = /[^+\/0-9A-Za-z-_]/g;
    CsvError = class extends Error {
      constructor(code, message, options, ...contexts) {
        if (Array.isArray(message))
          message = message.join(" ").trim();
        super(message);
        if (Error.captureStackTrace !== void 0) {
          Error.captureStackTrace(this, CsvError);
        }
        this.code = code;
        for (const context of contexts) {
          for (const key in context) {
            const value = context[key];
            this[key] = isBuffer(value) ? value.toString(options.encoding) : value == null ? value : JSON.parse(JSON.stringify(value));
          }
        }
      }
    };
    is_object = function(obj) {
      return typeof obj === "object" && obj !== null && !Array.isArray(obj);
    };
    normalize_columns_array = function(columns) {
      const normalizedColumns = [];
      for (let i = 0, l = columns.length; i < l; i++) {
        const column = columns[i];
        if (column === void 0 || column === null || column === false) {
          normalizedColumns[i] = { disabled: true };
        } else if (typeof column === "string") {
          normalizedColumns[i] = { name: column };
        } else if (is_object(column)) {
          if (typeof column.name !== "string") {
            throw new CsvError("CSV_OPTION_COLUMNS_MISSING_NAME", [
              "Option columns missing name:",
              `property "name" is required at position ${i}`,
              "when column is an object literal"
            ]);
          }
          normalizedColumns[i] = column;
        } else {
          throw new CsvError("CSV_INVALID_COLUMN_DEFINITION", [
            "Invalid column definition:",
            "expect a string or a literal object,",
            `got ${JSON.stringify(column)} at position ${i}`
          ]);
        }
      }
      return normalizedColumns;
    };
    ResizeableBuffer = class {
      constructor(size = 100) {
        this.size = size;
        this.length = 0;
        this.buf = Buffer2.allocUnsafe(size);
      }
      prepend(val) {
        if (isBuffer(val)) {
          const length = this.length + val.length;
          if (length >= this.size) {
            this.resize();
            if (length >= this.size) {
              throw Error("INVALID_BUFFER_STATE");
            }
          }
          const buf = this.buf;
          this.buf = Buffer2.allocUnsafe(this.size);
          val.copy(this.buf, 0);
          buf.copy(this.buf, val.length);
          this.length += val.length;
        } else {
          const length = this.length++;
          if (length === this.size) {
            this.resize();
          }
          const buf = this.clone();
          this.buf[0] = val;
          buf.copy(this.buf, 1, 0, length);
        }
      }
      append(val) {
        const length = this.length++;
        if (length === this.size) {
          this.resize();
        }
        this.buf[length] = val;
      }
      clone() {
        return Buffer2.from(this.buf.slice(0, this.length));
      }
      resize() {
        const length = this.length;
        this.size = this.size * 2;
        const buf = Buffer2.allocUnsafe(this.size);
        this.buf.copy(buf, 0, 0, length);
        this.buf = buf;
      }
      toString(encoding) {
        if (encoding) {
          return this.buf.slice(0, this.length).toString(encoding);
        } else {
          return Uint8Array.prototype.slice.call(this.buf.slice(0, this.length));
        }
      }
      toJSON() {
        return this.toString("utf8");
      }
      reset() {
        this.length = 0;
      }
    };
    np = 12;
    cr$1 = 13;
    nl$1 = 10;
    space = 32;
    tab = 9;
    init_state = function(options) {
      return {
        bomSkipped: false,
        bufBytesStart: 0,
        castField: options.cast_function,
        commenting: false,
        // Current error encountered by a record
        error: void 0,
        enabled: options.from_line === 1,
        escaping: false,
        escapeIsQuote: isBuffer(options.escape) && isBuffer(options.quote) && Buffer2.compare(options.escape, options.quote) === 0,
        // columns can be `false`, `true`, `Array`
        expectedRecordLength: Array.isArray(options.columns) ? options.columns.length : void 0,
        field: new ResizeableBuffer(20),
        firstLineToHeaders: options.cast_first_line_to_header,
        needMoreDataSize: Math.max(
          // Skip if the remaining buffer smaller than comment
          options.comment !== null ? options.comment.length : 0,
          ...options.delimiter.map((delimiter) => delimiter.length),
          // Skip if the remaining buffer can be escape sequence
          options.quote !== null ? options.quote.length : 0
        ),
        previousBuf: void 0,
        quoting: false,
        stop: false,
        rawBuffer: new ResizeableBuffer(100),
        record: [],
        recordHasError: false,
        record_length: 0,
        recordDelimiterMaxLength: options.record_delimiter.length === 0 ? 0 : Math.max(...options.record_delimiter.map((v) => v.length)),
        trimChars: [
          Buffer2.from(" ", options.encoding)[0],
          Buffer2.from("	", options.encoding)[0]
        ],
        wasQuoting: false,
        wasRowDelimiter: false,
        timchars: [
          Buffer2.from(Buffer2.from([cr$1], "utf8").toString(), options.encoding),
          Buffer2.from(Buffer2.from([nl$1], "utf8").toString(), options.encoding),
          Buffer2.from(Buffer2.from([np], "utf8").toString(), options.encoding),
          Buffer2.from(Buffer2.from([space], "utf8").toString(), options.encoding),
          Buffer2.from(Buffer2.from([tab], "utf8").toString(), options.encoding)
        ]
      };
    };
    underscore = function(str) {
      return str.replace(/([A-Z])/g, function(_, match) {
        return "_" + match.toLowerCase();
      });
    };
    normalize_options = function(opts) {
      const options = {};
      for (const opt in opts) {
        options[underscore(opt)] = opts[opt];
      }
      if (options.encoding === void 0 || options.encoding === true) {
        options.encoding = "utf8";
      } else if (options.encoding === null || options.encoding === false) {
        options.encoding = null;
      } else if (typeof options.encoding !== "string" && options.encoding !== null) {
        throw new CsvError(
          "CSV_INVALID_OPTION_ENCODING",
          [
            "Invalid option encoding:",
            "encoding must be a string or null to return a buffer,",
            `got ${JSON.stringify(options.encoding)}`
          ],
          options
        );
      }
      if (options.bom === void 0 || options.bom === null || options.bom === false) {
        options.bom = false;
      } else if (options.bom !== true) {
        throw new CsvError(
          "CSV_INVALID_OPTION_BOM",
          [
            "Invalid option bom:",
            "bom must be true,",
            `got ${JSON.stringify(options.bom)}`
          ],
          options
        );
      }
      options.cast_function = null;
      if (options.cast === void 0 || options.cast === null || options.cast === false || options.cast === "") {
        options.cast = void 0;
      } else if (typeof options.cast === "function") {
        options.cast_function = options.cast;
        options.cast = true;
      } else if (options.cast !== true) {
        throw new CsvError(
          "CSV_INVALID_OPTION_CAST",
          [
            "Invalid option cast:",
            "cast must be true or a function,",
            `got ${JSON.stringify(options.cast)}`
          ],
          options
        );
      }
      if (options.cast_date === void 0 || options.cast_date === null || options.cast_date === false || options.cast_date === "") {
        options.cast_date = false;
      } else if (options.cast_date === true) {
        options.cast_date = function(value) {
          const date = Date.parse(value);
          return !isNaN(date) ? new Date(date) : value;
        };
      } else if (typeof options.cast_date !== "function") {
        throw new CsvError(
          "CSV_INVALID_OPTION_CAST_DATE",
          [
            "Invalid option cast_date:",
            "cast_date must be true or a function,",
            `got ${JSON.stringify(options.cast_date)}`
          ],
          options
        );
      }
      options.cast_first_line_to_header = void 0;
      if (options.columns === true) {
        options.cast_first_line_to_header = void 0;
      } else if (typeof options.columns === "function") {
        options.cast_first_line_to_header = options.columns;
        options.columns = true;
      } else if (Array.isArray(options.columns)) {
        options.columns = normalize_columns_array(options.columns);
      } else if (options.columns === void 0 || options.columns === null || options.columns === false) {
        options.columns = false;
      } else {
        throw new CsvError(
          "CSV_INVALID_OPTION_COLUMNS",
          [
            "Invalid option columns:",
            "expect an array, a function or true,",
            `got ${JSON.stringify(options.columns)}`
          ],
          options
        );
      }
      if (options.group_columns_by_name === void 0 || options.group_columns_by_name === null || options.group_columns_by_name === false) {
        options.group_columns_by_name = false;
      } else if (options.group_columns_by_name !== true) {
        throw new CsvError(
          "CSV_INVALID_OPTION_GROUP_COLUMNS_BY_NAME",
          [
            "Invalid option group_columns_by_name:",
            "expect an boolean,",
            `got ${JSON.stringify(options.group_columns_by_name)}`
          ],
          options
        );
      } else if (options.columns === false) {
        throw new CsvError(
          "CSV_INVALID_OPTION_GROUP_COLUMNS_BY_NAME",
          [
            "Invalid option group_columns_by_name:",
            "the `columns` mode must be activated."
          ],
          options
        );
      }
      if (options.comment === void 0 || options.comment === null || options.comment === false || options.comment === "") {
        options.comment = null;
      } else {
        if (typeof options.comment === "string") {
          options.comment = Buffer2.from(options.comment, options.encoding);
        }
        if (!isBuffer(options.comment)) {
          throw new CsvError(
            "CSV_INVALID_OPTION_COMMENT",
            [
              "Invalid option comment:",
              "comment must be a buffer or a string,",
              `got ${JSON.stringify(options.comment)}`
            ],
            options
          );
        }
      }
      if (options.comment_no_infix === void 0 || options.comment_no_infix === null || options.comment_no_infix === false) {
        options.comment_no_infix = false;
      } else if (options.comment_no_infix !== true) {
        throw new CsvError(
          "CSV_INVALID_OPTION_COMMENT",
          [
            "Invalid option comment_no_infix:",
            "value must be a boolean,",
            `got ${JSON.stringify(options.comment_no_infix)}`
          ],
          options
        );
      }
      const delimiter_json = JSON.stringify(options.delimiter);
      if (!Array.isArray(options.delimiter))
        options.delimiter = [options.delimiter];
      if (options.delimiter.length === 0) {
        throw new CsvError(
          "CSV_INVALID_OPTION_DELIMITER",
          [
            "Invalid option delimiter:",
            "delimiter must be a non empty string or buffer or array of string|buffer,",
            `got ${delimiter_json}`
          ],
          options
        );
      }
      options.delimiter = options.delimiter.map(function(delimiter) {
        if (delimiter === void 0 || delimiter === null || delimiter === false) {
          return Buffer2.from(",", options.encoding);
        }
        if (typeof delimiter === "string") {
          delimiter = Buffer2.from(delimiter, options.encoding);
        }
        if (!isBuffer(delimiter) || delimiter.length === 0) {
          throw new CsvError(
            "CSV_INVALID_OPTION_DELIMITER",
            [
              "Invalid option delimiter:",
              "delimiter must be a non empty string or buffer or array of string|buffer,",
              `got ${delimiter_json}`
            ],
            options
          );
        }
        return delimiter;
      });
      if (options.escape === void 0 || options.escape === true) {
        options.escape = Buffer2.from('"', options.encoding);
      } else if (typeof options.escape === "string") {
        options.escape = Buffer2.from(options.escape, options.encoding);
      } else if (options.escape === null || options.escape === false) {
        options.escape = null;
      }
      if (options.escape !== null) {
        if (!isBuffer(options.escape)) {
          throw new Error(
            `Invalid Option: escape must be a buffer, a string or a boolean, got ${JSON.stringify(options.escape)}`
          );
        }
      }
      if (options.from === void 0 || options.from === null) {
        options.from = 1;
      } else {
        if (typeof options.from === "string" && /\d+/.test(options.from)) {
          options.from = parseInt(options.from);
        }
        if (Number.isInteger(options.from)) {
          if (options.from < 0) {
            throw new Error(
              `Invalid Option: from must be a positive integer, got ${JSON.stringify(opts.from)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: from must be an integer, got ${JSON.stringify(options.from)}`
          );
        }
      }
      if (options.from_line === void 0 || options.from_line === null) {
        options.from_line = 1;
      } else {
        if (typeof options.from_line === "string" && /\d+/.test(options.from_line)) {
          options.from_line = parseInt(options.from_line);
        }
        if (Number.isInteger(options.from_line)) {
          if (options.from_line <= 0) {
            throw new Error(
              `Invalid Option: from_line must be a positive integer greater than 0, got ${JSON.stringify(opts.from_line)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: from_line must be an integer, got ${JSON.stringify(opts.from_line)}`
          );
        }
      }
      if (options.ignore_last_delimiters === void 0 || options.ignore_last_delimiters === null) {
        options.ignore_last_delimiters = false;
      } else if (typeof options.ignore_last_delimiters === "number") {
        options.ignore_last_delimiters = Math.floor(options.ignore_last_delimiters);
        if (options.ignore_last_delimiters === 0) {
          options.ignore_last_delimiters = false;
        }
      } else if (typeof options.ignore_last_delimiters !== "boolean") {
        throw new CsvError(
          "CSV_INVALID_OPTION_IGNORE_LAST_DELIMITERS",
          [
            "Invalid option `ignore_last_delimiters`:",
            "the value must be a boolean value or an integer,",
            `got ${JSON.stringify(options.ignore_last_delimiters)}`
          ],
          options
        );
      }
      if (options.ignore_last_delimiters === true && options.columns === false) {
        throw new CsvError(
          "CSV_IGNORE_LAST_DELIMITERS_REQUIRES_COLUMNS",
          [
            "The option `ignore_last_delimiters`",
            "requires the activation of the `columns` option"
          ],
          options
        );
      }
      if (options.info === void 0 || options.info === null || options.info === false) {
        options.info = false;
      } else if (options.info !== true) {
        throw new Error(
          `Invalid Option: info must be true, got ${JSON.stringify(options.info)}`
        );
      }
      if (options.max_record_size === void 0 || options.max_record_size === null || options.max_record_size === false) {
        options.max_record_size = 0;
      } else if (Number.isInteger(options.max_record_size) && options.max_record_size >= 0)
        ;
      else if (typeof options.max_record_size === "string" && /\d+/.test(options.max_record_size)) {
        options.max_record_size = parseInt(options.max_record_size);
      } else {
        throw new Error(
          `Invalid Option: max_record_size must be a positive integer, got ${JSON.stringify(options.max_record_size)}`
        );
      }
      if (options.objname === void 0 || options.objname === null || options.objname === false) {
        options.objname = void 0;
      } else if (isBuffer(options.objname)) {
        if (options.objname.length === 0) {
          throw new Error(`Invalid Option: objname must be a non empty buffer`);
        }
        if (options.encoding === null)
          ;
        else {
          options.objname = options.objname.toString(options.encoding);
        }
      } else if (typeof options.objname === "string") {
        if (options.objname.length === 0) {
          throw new Error(`Invalid Option: objname must be a non empty string`);
        }
      } else if (typeof options.objname === "number")
        ;
      else {
        throw new Error(
          `Invalid Option: objname must be a string or a buffer, got ${options.objname}`
        );
      }
      if (options.objname !== void 0) {
        if (typeof options.objname === "number") {
          if (options.columns !== false) {
            throw Error(
              "Invalid Option: objname index cannot be combined with columns or be defined as a field"
            );
          }
        } else {
          if (options.columns === false) {
            throw Error(
              "Invalid Option: objname field must be combined with columns or be defined as an index"
            );
          }
        }
      }
      if (options.on_record === void 0 || options.on_record === null) {
        options.on_record = void 0;
      } else if (typeof options.on_record !== "function") {
        throw new CsvError(
          "CSV_INVALID_OPTION_ON_RECORD",
          [
            "Invalid option `on_record`:",
            "expect a function,",
            `got ${JSON.stringify(options.on_record)}`
          ],
          options
        );
      }
      if (options.on_skip !== void 0 && options.on_skip !== null && typeof options.on_skip !== "function") {
        throw new Error(
          `Invalid Option: on_skip must be a function, got ${JSON.stringify(options.on_skip)}`
        );
      }
      if (options.quote === null || options.quote === false || options.quote === "") {
        options.quote = null;
      } else {
        if (options.quote === void 0 || options.quote === true) {
          options.quote = Buffer2.from('"', options.encoding);
        } else if (typeof options.quote === "string") {
          options.quote = Buffer2.from(options.quote, options.encoding);
        }
        if (!isBuffer(options.quote)) {
          throw new Error(
            `Invalid Option: quote must be a buffer or a string, got ${JSON.stringify(options.quote)}`
          );
        }
      }
      if (options.raw === void 0 || options.raw === null || options.raw === false) {
        options.raw = false;
      } else if (options.raw !== true) {
        throw new Error(
          `Invalid Option: raw must be true, got ${JSON.stringify(options.raw)}`
        );
      }
      if (options.record_delimiter === void 0) {
        options.record_delimiter = [];
      } else if (typeof options.record_delimiter === "string" || isBuffer(options.record_delimiter)) {
        if (options.record_delimiter.length === 0) {
          throw new CsvError(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a non empty string or buffer,",
              `got ${JSON.stringify(options.record_delimiter)}`
            ],
            options
          );
        }
        options.record_delimiter = [options.record_delimiter];
      } else if (!Array.isArray(options.record_delimiter)) {
        throw new CsvError(
          "CSV_INVALID_OPTION_RECORD_DELIMITER",
          [
            "Invalid option `record_delimiter`:",
            "value must be a string, a buffer or array of string|buffer,",
            `got ${JSON.stringify(options.record_delimiter)}`
          ],
          options
        );
      }
      options.record_delimiter = options.record_delimiter.map(function(rd, i) {
        if (typeof rd !== "string" && !isBuffer(rd)) {
          throw new CsvError(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a string, a buffer or array of string|buffer",
              `at index ${i},`,
              `got ${JSON.stringify(rd)}`
            ],
            options
          );
        } else if (rd.length === 0) {
          throw new CsvError(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a non empty string or buffer",
              `at index ${i},`,
              `got ${JSON.stringify(rd)}`
            ],
            options
          );
        }
        if (typeof rd === "string") {
          rd = Buffer2.from(rd, options.encoding);
        }
        return rd;
      });
      if (typeof options.relax_column_count === "boolean")
        ;
      else if (options.relax_column_count === void 0 || options.relax_column_count === null) {
        options.relax_column_count = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count must be a boolean, got ${JSON.stringify(options.relax_column_count)}`
        );
      }
      if (typeof options.relax_column_count_less === "boolean")
        ;
      else if (options.relax_column_count_less === void 0 || options.relax_column_count_less === null) {
        options.relax_column_count_less = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count_less must be a boolean, got ${JSON.stringify(options.relax_column_count_less)}`
        );
      }
      if (typeof options.relax_column_count_more === "boolean")
        ;
      else if (options.relax_column_count_more === void 0 || options.relax_column_count_more === null) {
        options.relax_column_count_more = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count_more must be a boolean, got ${JSON.stringify(options.relax_column_count_more)}`
        );
      }
      if (typeof options.relax_quotes === "boolean")
        ;
      else if (options.relax_quotes === void 0 || options.relax_quotes === null) {
        options.relax_quotes = false;
      } else {
        throw new Error(
          `Invalid Option: relax_quotes must be a boolean, got ${JSON.stringify(options.relax_quotes)}`
        );
      }
      if (typeof options.skip_empty_lines === "boolean")
        ;
      else if (options.skip_empty_lines === void 0 || options.skip_empty_lines === null) {
        options.skip_empty_lines = false;
      } else {
        throw new Error(
          `Invalid Option: skip_empty_lines must be a boolean, got ${JSON.stringify(options.skip_empty_lines)}`
        );
      }
      if (typeof options.skip_records_with_empty_values === "boolean")
        ;
      else if (options.skip_records_with_empty_values === void 0 || options.skip_records_with_empty_values === null) {
        options.skip_records_with_empty_values = false;
      } else {
        throw new Error(
          `Invalid Option: skip_records_with_empty_values must be a boolean, got ${JSON.stringify(options.skip_records_with_empty_values)}`
        );
      }
      if (typeof options.skip_records_with_error === "boolean")
        ;
      else if (options.skip_records_with_error === void 0 || options.skip_records_with_error === null) {
        options.skip_records_with_error = false;
      } else {
        throw new Error(
          `Invalid Option: skip_records_with_error must be a boolean, got ${JSON.stringify(options.skip_records_with_error)}`
        );
      }
      if (options.rtrim === void 0 || options.rtrim === null || options.rtrim === false) {
        options.rtrim = false;
      } else if (options.rtrim !== true) {
        throw new Error(
          `Invalid Option: rtrim must be a boolean, got ${JSON.stringify(options.rtrim)}`
        );
      }
      if (options.ltrim === void 0 || options.ltrim === null || options.ltrim === false) {
        options.ltrim = false;
      } else if (options.ltrim !== true) {
        throw new Error(
          `Invalid Option: ltrim must be a boolean, got ${JSON.stringify(options.ltrim)}`
        );
      }
      if (options.trim === void 0 || options.trim === null || options.trim === false) {
        options.trim = false;
      } else if (options.trim !== true) {
        throw new Error(
          `Invalid Option: trim must be a boolean, got ${JSON.stringify(options.trim)}`
        );
      }
      if (options.trim === true && opts.ltrim !== false) {
        options.ltrim = true;
      } else if (options.ltrim !== true) {
        options.ltrim = false;
      }
      if (options.trim === true && opts.rtrim !== false) {
        options.rtrim = true;
      } else if (options.rtrim !== true) {
        options.rtrim = false;
      }
      if (options.to === void 0 || options.to === null) {
        options.to = -1;
      } else if (options.to !== -1) {
        if (typeof options.to === "string" && /\d+/.test(options.to)) {
          options.to = parseInt(options.to);
        }
        if (Number.isInteger(options.to)) {
          if (options.to <= 0) {
            throw new Error(
              `Invalid Option: to must be a positive integer greater than 0, got ${JSON.stringify(opts.to)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: to must be an integer, got ${JSON.stringify(opts.to)}`
          );
        }
      }
      if (options.to_line === void 0 || options.to_line === null) {
        options.to_line = -1;
      } else if (options.to_line !== -1) {
        if (typeof options.to_line === "string" && /\d+/.test(options.to_line)) {
          options.to_line = parseInt(options.to_line);
        }
        if (Number.isInteger(options.to_line)) {
          if (options.to_line <= 0) {
            throw new Error(
              `Invalid Option: to_line must be a positive integer greater than 0, got ${JSON.stringify(opts.to_line)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: to_line must be an integer, got ${JSON.stringify(opts.to_line)}`
          );
        }
      }
      return options;
    };
    isRecordEmpty = function(record) {
      return record.every(
        (field) => field == null || field.toString && field.toString().trim() === ""
      );
    };
    cr = 13;
    nl = 10;
    boms = {
      // Note, the following are equals:
      // Buffer.from("\ufeff")
      // Buffer.from([239, 187, 191])
      // Buffer.from('EFBBBF', 'hex')
      utf8: Buffer2.from([239, 187, 191]),
      // Note, the following are equals:
      // Buffer.from "\ufeff", 'utf16le
      // Buffer.from([255, 254])
      utf16le: Buffer2.from([255, 254])
    };
    transform = function(original_options = {}) {
      const info2 = {
        bytes: 0,
        comment_lines: 0,
        empty_lines: 0,
        invalid_field_length: 0,
        lines: 1,
        records: 0
      };
      const options = normalize_options(original_options);
      return {
        info: info2,
        original_options,
        options,
        state: init_state(options),
        __needMoreData: function(i, bufLen, end) {
          if (end)
            return false;
          const { encoding, escape, quote } = this.options;
          const { quoting, needMoreDataSize, recordDelimiterMaxLength } = this.state;
          const numOfCharLeft = bufLen - i - 1;
          const requiredLength = Math.max(
            needMoreDataSize,
            // Skip if the remaining buffer smaller than record delimiter
            // If "record_delimiter" is yet to be discovered:
            // 1. It is equals to `[]` and "recordDelimiterMaxLength" equals `0`
            // 2. We set the length to windows line ending in the current encoding
            // Note, that encoding is known from user or bom discovery at that point
            // recordDelimiterMaxLength,
            recordDelimiterMaxLength === 0 ? Buffer2.from("\r\n", encoding).length : recordDelimiterMaxLength,
            // Skip if remaining buffer can be an escaped quote
            quoting ? (escape === null ? 0 : escape.length) + quote.length : 0,
            // Skip if remaining buffer can be record delimiter following the closing quote
            quoting ? quote.length + recordDelimiterMaxLength : 0
          );
          return numOfCharLeft < requiredLength;
        },
        // Central parser implementation
        parse: function(nextBuf, end, push, close) {
          const {
            bom,
            comment_no_infix,
            encoding,
            from_line,
            ltrim,
            max_record_size,
            raw,
            relax_quotes,
            rtrim,
            skip_empty_lines,
            to,
            to_line
          } = this.options;
          let { comment, escape, quote, record_delimiter } = this.options;
          const { bomSkipped, previousBuf, rawBuffer, escapeIsQuote } = this.state;
          let buf;
          if (previousBuf === void 0) {
            if (nextBuf === void 0) {
              close();
              return;
            } else {
              buf = nextBuf;
            }
          } else if (previousBuf !== void 0 && nextBuf === void 0) {
            buf = previousBuf;
          } else {
            buf = Buffer2.concat([previousBuf, nextBuf]);
          }
          if (bomSkipped === false) {
            if (bom === false) {
              this.state.bomSkipped = true;
            } else if (buf.length < 3) {
              if (end === false) {
                this.state.previousBuf = buf;
                return;
              }
            } else {
              for (const encoding2 in boms) {
                if (boms[encoding2].compare(buf, 0, boms[encoding2].length) === 0) {
                  const bomLength = boms[encoding2].length;
                  this.state.bufBytesStart += bomLength;
                  buf = buf.slice(bomLength);
                  const options2 = normalize_options({
                    ...this.original_options,
                    encoding: encoding2
                  });
                  for (const key in options2) {
                    this.options[key] = options2[key];
                  }
                  ({ comment, escape, quote } = this.options);
                  break;
                }
              }
              this.state.bomSkipped = true;
            }
          }
          const bufLen = buf.length;
          let pos;
          for (pos = 0; pos < bufLen; pos++) {
            if (this.__needMoreData(pos, bufLen, end)) {
              break;
            }
            if (this.state.wasRowDelimiter === true) {
              this.info.lines++;
              this.state.wasRowDelimiter = false;
            }
            if (to_line !== -1 && this.info.lines > to_line) {
              this.state.stop = true;
              close();
              return;
            }
            if (this.state.quoting === false && record_delimiter.length === 0) {
              const record_delimiterCount = this.__autoDiscoverRecordDelimiter(
                buf,
                pos
              );
              if (record_delimiterCount) {
                record_delimiter = this.options.record_delimiter;
              }
            }
            const chr = buf[pos];
            if (raw === true) {
              rawBuffer.append(chr);
            }
            if ((chr === cr || chr === nl) && this.state.wasRowDelimiter === false) {
              this.state.wasRowDelimiter = true;
            }
            if (this.state.escaping === true) {
              this.state.escaping = false;
            } else {
              if (escape !== null && this.state.quoting === true && this.__isEscape(buf, pos, chr) && pos + escape.length < bufLen) {
                if (escapeIsQuote) {
                  if (this.__isQuote(buf, pos + escape.length)) {
                    this.state.escaping = true;
                    pos += escape.length - 1;
                    continue;
                  }
                } else {
                  this.state.escaping = true;
                  pos += escape.length - 1;
                  continue;
                }
              }
              if (this.state.commenting === false && this.__isQuote(buf, pos)) {
                if (this.state.quoting === true) {
                  const nextChr = buf[pos + quote.length];
                  const isNextChrTrimable = rtrim && this.__isCharTrimable(buf, pos + quote.length);
                  const isNextChrComment = comment !== null && this.__compareBytes(comment, buf, pos + quote.length, nextChr);
                  const isNextChrDelimiter = this.__isDelimiter(
                    buf,
                    pos + quote.length,
                    nextChr
                  );
                  const isNextChrRecordDelimiter = record_delimiter.length === 0 ? this.__autoDiscoverRecordDelimiter(buf, pos + quote.length) : this.__isRecordDelimiter(nextChr, buf, pos + quote.length);
                  if (escape !== null && this.__isEscape(buf, pos, chr) && this.__isQuote(buf, pos + escape.length)) {
                    pos += escape.length - 1;
                  } else if (!nextChr || isNextChrDelimiter || isNextChrRecordDelimiter || isNextChrComment || isNextChrTrimable) {
                    this.state.quoting = false;
                    this.state.wasQuoting = true;
                    pos += quote.length - 1;
                    continue;
                  } else if (relax_quotes === false) {
                    const err = this.__error(
                      new CsvError(
                        "CSV_INVALID_CLOSING_QUOTE",
                        [
                          "Invalid Closing Quote:",
                          `got "${String.fromCharCode(nextChr)}"`,
                          `at line ${this.info.lines}`,
                          "instead of delimiter, record delimiter, trimable character",
                          "(if activated) or comment"
                        ],
                        this.options,
                        this.__infoField()
                      )
                    );
                    if (err !== void 0)
                      return err;
                  } else {
                    this.state.quoting = false;
                    this.state.wasQuoting = true;
                    this.state.field.prepend(quote);
                    pos += quote.length - 1;
                  }
                } else {
                  if (this.state.field.length !== 0) {
                    if (relax_quotes === false) {
                      const info3 = this.__infoField();
                      const bom2 = Object.keys(boms).map(
                        (b) => boms[b].equals(this.state.field.toString()) ? b : false
                      ).filter(Boolean)[0];
                      const err = this.__error(
                        new CsvError(
                          "INVALID_OPENING_QUOTE",
                          [
                            "Invalid Opening Quote:",
                            `a quote is found on field ${JSON.stringify(info3.column)} at line ${info3.lines}, value is ${JSON.stringify(this.state.field.toString(encoding))}`,
                            bom2 ? `(${bom2} bom)` : void 0
                          ],
                          this.options,
                          info3,
                          {
                            field: this.state.field
                          }
                        )
                      );
                      if (err !== void 0)
                        return err;
                    }
                  } else {
                    this.state.quoting = true;
                    pos += quote.length - 1;
                    continue;
                  }
                }
              }
              if (this.state.quoting === false) {
                const recordDelimiterLength = this.__isRecordDelimiter(
                  chr,
                  buf,
                  pos
                );
                if (recordDelimiterLength !== 0) {
                  const skipCommentLine = this.state.commenting && this.state.wasQuoting === false && this.state.record.length === 0 && this.state.field.length === 0;
                  if (skipCommentLine) {
                    this.info.comment_lines++;
                  } else {
                    if (this.state.enabled === false && this.info.lines + (this.state.wasRowDelimiter === true ? 1 : 0) >= from_line) {
                      this.state.enabled = true;
                      this.__resetField();
                      this.__resetRecord();
                      pos += recordDelimiterLength - 1;
                      continue;
                    }
                    if (skip_empty_lines === true && this.state.wasQuoting === false && this.state.record.length === 0 && this.state.field.length === 0) {
                      this.info.empty_lines++;
                      pos += recordDelimiterLength - 1;
                      continue;
                    }
                    this.info.bytes = this.state.bufBytesStart + pos;
                    const errField = this.__onField();
                    if (errField !== void 0)
                      return errField;
                    this.info.bytes = this.state.bufBytesStart + pos + recordDelimiterLength;
                    const errRecord = this.__onRecord(push);
                    if (errRecord !== void 0)
                      return errRecord;
                    if (to !== -1 && this.info.records >= to) {
                      this.state.stop = true;
                      close();
                      return;
                    }
                  }
                  this.state.commenting = false;
                  pos += recordDelimiterLength - 1;
                  continue;
                }
                if (this.state.commenting) {
                  continue;
                }
                if (comment !== null && (comment_no_infix === false || this.state.record.length === 0 && this.state.field.length === 0)) {
                  const commentCount = this.__compareBytes(comment, buf, pos, chr);
                  if (commentCount !== 0) {
                    this.state.commenting = true;
                    continue;
                  }
                }
                const delimiterLength = this.__isDelimiter(buf, pos, chr);
                if (delimiterLength !== 0) {
                  this.info.bytes = this.state.bufBytesStart + pos;
                  const errField = this.__onField();
                  if (errField !== void 0)
                    return errField;
                  pos += delimiterLength - 1;
                  continue;
                }
              }
            }
            if (this.state.commenting === false) {
              if (max_record_size !== 0 && this.state.record_length + this.state.field.length > max_record_size) {
                return this.__error(
                  new CsvError(
                    "CSV_MAX_RECORD_SIZE",
                    [
                      "Max Record Size:",
                      "record exceed the maximum number of tolerated bytes",
                      `of ${max_record_size}`,
                      `at line ${this.info.lines}`
                    ],
                    this.options,
                    this.__infoField()
                  )
                );
              }
            }
            const lappend = ltrim === false || this.state.quoting === true || this.state.field.length !== 0 || !this.__isCharTrimable(buf, pos);
            const rappend = rtrim === false || this.state.wasQuoting === false;
            if (lappend === true && rappend === true) {
              this.state.field.append(chr);
            } else if (rtrim === true && !this.__isCharTrimable(buf, pos)) {
              return this.__error(
                new CsvError(
                  "CSV_NON_TRIMABLE_CHAR_AFTER_CLOSING_QUOTE",
                  [
                    "Invalid Closing Quote:",
                    "found non trimable byte after quote",
                    `at line ${this.info.lines}`
                  ],
                  this.options,
                  this.__infoField()
                )
              );
            } else {
              if (lappend === false) {
                pos += this.__isCharTrimable(buf, pos) - 1;
              }
              continue;
            }
          }
          if (end === true) {
            if (this.state.quoting === true) {
              const err = this.__error(
                new CsvError(
                  "CSV_QUOTE_NOT_CLOSED",
                  [
                    "Quote Not Closed:",
                    `the parsing is finished with an opening quote at line ${this.info.lines}`
                  ],
                  this.options,
                  this.__infoField()
                )
              );
              if (err !== void 0)
                return err;
            } else {
              if (this.state.wasQuoting === true || this.state.record.length !== 0 || this.state.field.length !== 0) {
                this.info.bytes = this.state.bufBytesStart + pos;
                const errField = this.__onField();
                if (errField !== void 0)
                  return errField;
                const errRecord = this.__onRecord(push);
                if (errRecord !== void 0)
                  return errRecord;
              } else if (this.state.wasRowDelimiter === true) {
                this.info.empty_lines++;
              } else if (this.state.commenting === true) {
                this.info.comment_lines++;
              }
            }
          } else {
            this.state.bufBytesStart += pos;
            this.state.previousBuf = buf.slice(pos);
          }
          if (this.state.wasRowDelimiter === true) {
            this.info.lines++;
            this.state.wasRowDelimiter = false;
          }
        },
        __onRecord: function(push) {
          const {
            columns,
            group_columns_by_name,
            encoding,
            info: info3,
            from: from2,
            relax_column_count,
            relax_column_count_less,
            relax_column_count_more,
            raw,
            skip_records_with_empty_values
          } = this.options;
          const { enabled, record } = this.state;
          if (enabled === false) {
            return this.__resetRecord();
          }
          const recordLength = record.length;
          if (columns === true) {
            if (skip_records_with_empty_values === true && isRecordEmpty(record)) {
              this.__resetRecord();
              return;
            }
            return this.__firstLineToColumns(record);
          }
          if (columns === false && this.info.records === 0) {
            this.state.expectedRecordLength = recordLength;
          }
          if (recordLength !== this.state.expectedRecordLength) {
            const err = columns === false ? new CsvError(
              "CSV_RECORD_INCONSISTENT_FIELDS_LENGTH",
              [
                "Invalid Record Length:",
                `expect ${this.state.expectedRecordLength},`,
                `got ${recordLength} on line ${this.info.lines}`
              ],
              this.options,
              this.__infoField(),
              {
                record
              }
            ) : new CsvError(
              "CSV_RECORD_INCONSISTENT_COLUMNS",
              [
                "Invalid Record Length:",
                `columns length is ${columns.length},`,
                // rename columns
                `got ${recordLength} on line ${this.info.lines}`
              ],
              this.options,
              this.__infoField(),
              {
                record
              }
            );
            if (relax_column_count === true || relax_column_count_less === true && recordLength < this.state.expectedRecordLength || relax_column_count_more === true && recordLength > this.state.expectedRecordLength) {
              this.info.invalid_field_length++;
              this.state.error = err;
            } else {
              const finalErr = this.__error(err);
              if (finalErr)
                return finalErr;
            }
          }
          if (skip_records_with_empty_values === true && isRecordEmpty(record)) {
            this.__resetRecord();
            return;
          }
          if (this.state.recordHasError === true) {
            this.__resetRecord();
            this.state.recordHasError = false;
            return;
          }
          this.info.records++;
          if (from2 === 1 || this.info.records >= from2) {
            const { objname } = this.options;
            if (columns !== false) {
              const obj = {};
              for (let i = 0, l = record.length; i < l; i++) {
                if (columns[i] === void 0 || columns[i].disabled)
                  continue;
                if (group_columns_by_name === true && obj[columns[i].name] !== void 0) {
                  if (Array.isArray(obj[columns[i].name])) {
                    obj[columns[i].name] = obj[columns[i].name].concat(record[i]);
                  } else {
                    obj[columns[i].name] = [obj[columns[i].name], record[i]];
                  }
                } else {
                  obj[columns[i].name] = record[i];
                }
              }
              if (raw === true || info3 === true) {
                const extRecord = Object.assign(
                  { record: obj },
                  raw === true ? { raw: this.state.rawBuffer.toString(encoding) } : {},
                  info3 === true ? { info: this.__infoRecord() } : {}
                );
                const err = this.__push(
                  objname === void 0 ? extRecord : [obj[objname], extRecord],
                  push
                );
                if (err) {
                  return err;
                }
              } else {
                const err = this.__push(
                  objname === void 0 ? obj : [obj[objname], obj],
                  push
                );
                if (err) {
                  return err;
                }
              }
            } else {
              if (raw === true || info3 === true) {
                const extRecord = Object.assign(
                  { record },
                  raw === true ? { raw: this.state.rawBuffer.toString(encoding) } : {},
                  info3 === true ? { info: this.__infoRecord() } : {}
                );
                const err = this.__push(
                  objname === void 0 ? extRecord : [record[objname], extRecord],
                  push
                );
                if (err) {
                  return err;
                }
              } else {
                const err = this.__push(
                  objname === void 0 ? record : [record[objname], record],
                  push
                );
                if (err) {
                  return err;
                }
              }
            }
          }
          this.__resetRecord();
        },
        __firstLineToColumns: function(record) {
          const { firstLineToHeaders } = this.state;
          try {
            const headers = firstLineToHeaders === void 0 ? record : firstLineToHeaders.call(null, record);
            if (!Array.isArray(headers)) {
              return this.__error(
                new CsvError(
                  "CSV_INVALID_COLUMN_MAPPING",
                  [
                    "Invalid Column Mapping:",
                    "expect an array from column function,",
                    `got ${JSON.stringify(headers)}`
                  ],
                  this.options,
                  this.__infoField(),
                  {
                    headers
                  }
                )
              );
            }
            const normalizedHeaders = normalize_columns_array(headers);
            this.state.expectedRecordLength = normalizedHeaders.length;
            this.options.columns = normalizedHeaders;
            this.__resetRecord();
            return;
          } catch (err) {
            return err;
          }
        },
        __resetRecord: function() {
          if (this.options.raw === true) {
            this.state.rawBuffer.reset();
          }
          this.state.error = void 0;
          this.state.record = [];
          this.state.record_length = 0;
        },
        __onField: function() {
          const { cast, encoding, rtrim, max_record_size } = this.options;
          const { enabled, wasQuoting } = this.state;
          if (enabled === false) {
            return this.__resetField();
          }
          let field = this.state.field.toString(encoding);
          if (rtrim === true && wasQuoting === false) {
            field = field.trimRight();
          }
          if (cast === true) {
            const [err, f] = this.__cast(field);
            if (err !== void 0)
              return err;
            field = f;
          }
          this.state.record.push(field);
          if (max_record_size !== 0 && typeof field === "string") {
            this.state.record_length += field.length;
          }
          this.__resetField();
        },
        __resetField: function() {
          this.state.field.reset();
          this.state.wasQuoting = false;
        },
        __push: function(record, push) {
          const { on_record } = this.options;
          if (on_record !== void 0) {
            const info3 = this.__infoRecord();
            try {
              record = on_record.call(null, record, info3);
            } catch (err) {
              return err;
            }
            if (record === void 0 || record === null) {
              return;
            }
          }
          push(record);
        },
        // Return a tuple with the error and the casted value
        __cast: function(field) {
          const { columns, relax_column_count } = this.options;
          const isColumns = Array.isArray(columns);
          if (isColumns === true && relax_column_count && this.options.columns.length <= this.state.record.length) {
            return [void 0, void 0];
          }
          if (this.state.castField !== null) {
            try {
              const info3 = this.__infoField();
              return [void 0, this.state.castField.call(null, field, info3)];
            } catch (err) {
              return [err];
            }
          }
          if (this.__isFloat(field)) {
            return [void 0, parseFloat(field)];
          } else if (this.options.cast_date !== false) {
            const info3 = this.__infoField();
            return [void 0, this.options.cast_date.call(null, field, info3)];
          }
          return [void 0, field];
        },
        // Helper to test if a character is a space or a line delimiter
        __isCharTrimable: function(buf, pos) {
          const isTrim = (buf2, pos2) => {
            const { timchars } = this.state;
            loop1:
              for (let i = 0; i < timchars.length; i++) {
                const timchar = timchars[i];
                for (let j = 0; j < timchar.length; j++) {
                  if (timchar[j] !== buf2[pos2 + j])
                    continue loop1;
                }
                return timchar.length;
              }
            return 0;
          };
          return isTrim(buf, pos);
        },
        // Keep it in case we implement the `cast_int` option
        // __isInt(value){
        //   // return Number.isInteger(parseInt(value))
        //   // return !isNaN( parseInt( obj ) );
        //   return /^(\-|\+)?[1-9][0-9]*$/.test(value)
        // }
        __isFloat: function(value) {
          return value - parseFloat(value) + 1 >= 0;
        },
        __compareBytes: function(sourceBuf, targetBuf, targetPos, firstByte) {
          if (sourceBuf[0] !== firstByte)
            return 0;
          const sourceLength = sourceBuf.length;
          for (let i = 1; i < sourceLength; i++) {
            if (sourceBuf[i] !== targetBuf[targetPos + i])
              return 0;
          }
          return sourceLength;
        },
        __isDelimiter: function(buf, pos, chr) {
          const { delimiter, ignore_last_delimiters } = this.options;
          if (ignore_last_delimiters === true && this.state.record.length === this.options.columns.length - 1) {
            return 0;
          } else if (ignore_last_delimiters !== false && typeof ignore_last_delimiters === "number" && this.state.record.length === ignore_last_delimiters - 1) {
            return 0;
          }
          loop1:
            for (let i = 0; i < delimiter.length; i++) {
              const del = delimiter[i];
              if (del[0] === chr) {
                for (let j = 1; j < del.length; j++) {
                  if (del[j] !== buf[pos + j])
                    continue loop1;
                }
                return del.length;
              }
            }
          return 0;
        },
        __isRecordDelimiter: function(chr, buf, pos) {
          const { record_delimiter } = this.options;
          const recordDelimiterLength = record_delimiter.length;
          loop1:
            for (let i = 0; i < recordDelimiterLength; i++) {
              const rd = record_delimiter[i];
              const rdLength = rd.length;
              if (rd[0] !== chr) {
                continue;
              }
              for (let j = 1; j < rdLength; j++) {
                if (rd[j] !== buf[pos + j]) {
                  continue loop1;
                }
              }
              return rd.length;
            }
          return 0;
        },
        __isEscape: function(buf, pos, chr) {
          const { escape } = this.options;
          if (escape === null)
            return false;
          const l = escape.length;
          if (escape[0] === chr) {
            for (let i = 0; i < l; i++) {
              if (escape[i] !== buf[pos + i]) {
                return false;
              }
            }
            return true;
          }
          return false;
        },
        __isQuote: function(buf, pos) {
          const { quote } = this.options;
          if (quote === null)
            return false;
          const l = quote.length;
          for (let i = 0; i < l; i++) {
            if (quote[i] !== buf[pos + i]) {
              return false;
            }
          }
          return true;
        },
        __autoDiscoverRecordDelimiter: function(buf, pos) {
          const { encoding } = this.options;
          const rds = [
            // Important, the windows line ending must be before mac os 9
            Buffer2.from("\r\n", encoding),
            Buffer2.from("\n", encoding),
            Buffer2.from("\r", encoding)
          ];
          loop:
            for (let i = 0; i < rds.length; i++) {
              const l = rds[i].length;
              for (let j = 0; j < l; j++) {
                if (rds[i][j] !== buf[pos + j]) {
                  continue loop;
                }
              }
              this.options.record_delimiter.push(rds[i]);
              this.state.recordDelimiterMaxLength = rds[i].length;
              return rds[i].length;
            }
          return 0;
        },
        __error: function(msg) {
          const { encoding, raw, skip_records_with_error } = this.options;
          const err = typeof msg === "string" ? new Error(msg) : msg;
          if (skip_records_with_error) {
            this.state.recordHasError = true;
            if (this.options.on_skip !== void 0) {
              try {
                this.options.on_skip(
                  err,
                  raw ? this.state.rawBuffer.toString(encoding) : void 0
                );
              } catch (err2) {
                return err2;
              }
            }
            return void 0;
          } else {
            return err;
          }
        },
        __infoDataSet: function() {
          return {
            ...this.info,
            columns: this.options.columns
          };
        },
        __infoRecord: function() {
          const { columns, raw, encoding } = this.options;
          return {
            ...this.__infoDataSet(),
            error: this.state.error,
            header: columns === true,
            index: this.state.record.length,
            raw: raw ? this.state.rawBuffer.toString(encoding) : void 0
          };
        },
        __infoField: function() {
          const { columns } = this.options;
          const isColumns = Array.isArray(columns);
          return {
            ...this.__infoRecord(),
            column: isColumns === true ? columns.length > this.state.record.length ? columns[this.state.record.length].name : null : this.state.record.length,
            quoting: this.state.wasQuoting
          };
        }
      };
    };
    parse = function(data, opts = {}) {
      if (typeof data === "string") {
        data = Buffer2.from(data);
      }
      const records = opts && opts.objname ? {} : [];
      const parser = transform(opts);
      const push = (record) => {
        if (parser.options.objname === void 0)
          records.push(record);
        else {
          records[record[0]] = record[1];
        }
      };
      const close = () => {
      };
      const error2 = parser.parse(data, true, push, close);
      if (error2 !== void 0)
        throw error2;
      return records;
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/CsvError.js
var CsvError2;
var init_CsvError = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/CsvError.js"() {
    CsvError2 = class extends Error {
      constructor(code, message, options, ...contexts) {
        if (Array.isArray(message))
          message = message.join(" ").trim();
        super(message);
        if (Error.captureStackTrace !== void 0) {
          Error.captureStackTrace(this, CsvError2);
        }
        this.code = code;
        for (const context of contexts) {
          for (const key in context) {
            const value = context[key];
            this[key] = Buffer.isBuffer(value) ? value.toString(options.encoding) : value == null ? value : JSON.parse(JSON.stringify(value));
          }
        }
      }
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/is_object.js
var is_object2;
var init_is_object = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/is_object.js"() {
    is_object2 = function(obj) {
      return typeof obj === "object" && obj !== null && !Array.isArray(obj);
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/normalize_columns_array.js
var normalize_columns_array2;
var init_normalize_columns_array = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/normalize_columns_array.js"() {
    init_CsvError();
    init_is_object();
    normalize_columns_array2 = function(columns) {
      const normalizedColumns = [];
      for (let i = 0, l = columns.length; i < l; i++) {
        const column = columns[i];
        if (column === void 0 || column === null || column === false) {
          normalizedColumns[i] = { disabled: true };
        } else if (typeof column === "string") {
          normalizedColumns[i] = { name: column };
        } else if (is_object2(column)) {
          if (typeof column.name !== "string") {
            throw new CsvError2("CSV_OPTION_COLUMNS_MISSING_NAME", [
              "Option columns missing name:",
              `property "name" is required at position ${i}`,
              "when column is an object literal"
            ]);
          }
          normalizedColumns[i] = column;
        } else {
          throw new CsvError2("CSV_INVALID_COLUMN_DEFINITION", [
            "Invalid column definition:",
            "expect a string or a literal object,",
            `got ${JSON.stringify(column)} at position ${i}`
          ]);
        }
      }
      return normalizedColumns;
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/ResizeableBuffer.js
var ResizeableBuffer2, ResizeableBuffer_default;
var init_ResizeableBuffer = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/ResizeableBuffer.js"() {
    ResizeableBuffer2 = class {
      constructor(size = 100) {
        this.size = size;
        this.length = 0;
        this.buf = Buffer.allocUnsafe(size);
      }
      prepend(val) {
        if (Buffer.isBuffer(val)) {
          const length = this.length + val.length;
          if (length >= this.size) {
            this.resize();
            if (length >= this.size) {
              throw Error("INVALID_BUFFER_STATE");
            }
          }
          const buf = this.buf;
          this.buf = Buffer.allocUnsafe(this.size);
          val.copy(this.buf, 0);
          buf.copy(this.buf, val.length);
          this.length += val.length;
        } else {
          const length = this.length++;
          if (length === this.size) {
            this.resize();
          }
          const buf = this.clone();
          this.buf[0] = val;
          buf.copy(this.buf, 1, 0, length);
        }
      }
      append(val) {
        const length = this.length++;
        if (length === this.size) {
          this.resize();
        }
        this.buf[length] = val;
      }
      clone() {
        return Buffer.from(this.buf.slice(0, this.length));
      }
      resize() {
        const length = this.length;
        this.size = this.size * 2;
        const buf = Buffer.allocUnsafe(this.size);
        this.buf.copy(buf, 0, 0, length);
        this.buf = buf;
      }
      toString(encoding) {
        if (encoding) {
          return this.buf.slice(0, this.length).toString(encoding);
        } else {
          return Uint8Array.prototype.slice.call(this.buf.slice(0, this.length));
        }
      }
      toJSON() {
        return this.toString("utf8");
      }
      reset() {
        this.length = 0;
      }
    };
    ResizeableBuffer_default = ResizeableBuffer2;
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/init_state.js
var np2, cr2, nl2, space2, tab2, init_state2;
var init_init_state = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/init_state.js"() {
    init_ResizeableBuffer();
    np2 = 12;
    cr2 = 13;
    nl2 = 10;
    space2 = 32;
    tab2 = 9;
    init_state2 = function(options) {
      return {
        bomSkipped: false,
        bufBytesStart: 0,
        castField: options.cast_function,
        commenting: false,
        // Current error encountered by a record
        error: void 0,
        enabled: options.from_line === 1,
        escaping: false,
        escapeIsQuote: Buffer.isBuffer(options.escape) && Buffer.isBuffer(options.quote) && Buffer.compare(options.escape, options.quote) === 0,
        // columns can be `false`, `true`, `Array`
        expectedRecordLength: Array.isArray(options.columns) ? options.columns.length : void 0,
        field: new ResizeableBuffer_default(20),
        firstLineToHeaders: options.cast_first_line_to_header,
        needMoreDataSize: Math.max(
          // Skip if the remaining buffer smaller than comment
          options.comment !== null ? options.comment.length : 0,
          ...options.delimiter.map((delimiter) => delimiter.length),
          // Skip if the remaining buffer can be escape sequence
          options.quote !== null ? options.quote.length : 0
        ),
        previousBuf: void 0,
        quoting: false,
        stop: false,
        rawBuffer: new ResizeableBuffer_default(100),
        record: [],
        recordHasError: false,
        record_length: 0,
        recordDelimiterMaxLength: options.record_delimiter.length === 0 ? 0 : Math.max(...options.record_delimiter.map((v) => v.length)),
        trimChars: [
          Buffer.from(" ", options.encoding)[0],
          Buffer.from("	", options.encoding)[0]
        ],
        wasQuoting: false,
        wasRowDelimiter: false,
        timchars: [
          Buffer.from(Buffer.from([cr2], "utf8").toString(), options.encoding),
          Buffer.from(Buffer.from([nl2], "utf8").toString(), options.encoding),
          Buffer.from(Buffer.from([np2], "utf8").toString(), options.encoding),
          Buffer.from(Buffer.from([space2], "utf8").toString(), options.encoding),
          Buffer.from(Buffer.from([tab2], "utf8").toString(), options.encoding)
        ]
      };
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/underscore.js
var underscore2;
var init_underscore = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/utils/underscore.js"() {
    underscore2 = function(str) {
      return str.replace(/([A-Z])/g, function(_, match) {
        return "_" + match.toLowerCase();
      });
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/normalize_options.js
var normalize_options2;
var init_normalize_options = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/normalize_options.js"() {
    init_normalize_columns_array();
    init_CsvError();
    init_underscore();
    normalize_options2 = function(opts) {
      const options = {};
      for (const opt in opts) {
        options[underscore2(opt)] = opts[opt];
      }
      if (options.encoding === void 0 || options.encoding === true) {
        options.encoding = "utf8";
      } else if (options.encoding === null || options.encoding === false) {
        options.encoding = null;
      } else if (typeof options.encoding !== "string" && options.encoding !== null) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_ENCODING",
          [
            "Invalid option encoding:",
            "encoding must be a string or null to return a buffer,",
            `got ${JSON.stringify(options.encoding)}`
          ],
          options
        );
      }
      if (options.bom === void 0 || options.bom === null || options.bom === false) {
        options.bom = false;
      } else if (options.bom !== true) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_BOM",
          [
            "Invalid option bom:",
            "bom must be true,",
            `got ${JSON.stringify(options.bom)}`
          ],
          options
        );
      }
      options.cast_function = null;
      if (options.cast === void 0 || options.cast === null || options.cast === false || options.cast === "") {
        options.cast = void 0;
      } else if (typeof options.cast === "function") {
        options.cast_function = options.cast;
        options.cast = true;
      } else if (options.cast !== true) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_CAST",
          [
            "Invalid option cast:",
            "cast must be true or a function,",
            `got ${JSON.stringify(options.cast)}`
          ],
          options
        );
      }
      if (options.cast_date === void 0 || options.cast_date === null || options.cast_date === false || options.cast_date === "") {
        options.cast_date = false;
      } else if (options.cast_date === true) {
        options.cast_date = function(value) {
          const date = Date.parse(value);
          return !isNaN(date) ? new Date(date) : value;
        };
      } else if (typeof options.cast_date !== "function") {
        throw new CsvError2(
          "CSV_INVALID_OPTION_CAST_DATE",
          [
            "Invalid option cast_date:",
            "cast_date must be true or a function,",
            `got ${JSON.stringify(options.cast_date)}`
          ],
          options
        );
      }
      options.cast_first_line_to_header = void 0;
      if (options.columns === true) {
        options.cast_first_line_to_header = void 0;
      } else if (typeof options.columns === "function") {
        options.cast_first_line_to_header = options.columns;
        options.columns = true;
      } else if (Array.isArray(options.columns)) {
        options.columns = normalize_columns_array2(options.columns);
      } else if (options.columns === void 0 || options.columns === null || options.columns === false) {
        options.columns = false;
      } else {
        throw new CsvError2(
          "CSV_INVALID_OPTION_COLUMNS",
          [
            "Invalid option columns:",
            "expect an array, a function or true,",
            `got ${JSON.stringify(options.columns)}`
          ],
          options
        );
      }
      if (options.group_columns_by_name === void 0 || options.group_columns_by_name === null || options.group_columns_by_name === false) {
        options.group_columns_by_name = false;
      } else if (options.group_columns_by_name !== true) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_GROUP_COLUMNS_BY_NAME",
          [
            "Invalid option group_columns_by_name:",
            "expect an boolean,",
            `got ${JSON.stringify(options.group_columns_by_name)}`
          ],
          options
        );
      } else if (options.columns === false) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_GROUP_COLUMNS_BY_NAME",
          [
            "Invalid option group_columns_by_name:",
            "the `columns` mode must be activated."
          ],
          options
        );
      }
      if (options.comment === void 0 || options.comment === null || options.comment === false || options.comment === "") {
        options.comment = null;
      } else {
        if (typeof options.comment === "string") {
          options.comment = Buffer.from(options.comment, options.encoding);
        }
        if (!Buffer.isBuffer(options.comment)) {
          throw new CsvError2(
            "CSV_INVALID_OPTION_COMMENT",
            [
              "Invalid option comment:",
              "comment must be a buffer or a string,",
              `got ${JSON.stringify(options.comment)}`
            ],
            options
          );
        }
      }
      if (options.comment_no_infix === void 0 || options.comment_no_infix === null || options.comment_no_infix === false) {
        options.comment_no_infix = false;
      } else if (options.comment_no_infix !== true) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_COMMENT",
          [
            "Invalid option comment_no_infix:",
            "value must be a boolean,",
            `got ${JSON.stringify(options.comment_no_infix)}`
          ],
          options
        );
      }
      const delimiter_json = JSON.stringify(options.delimiter);
      if (!Array.isArray(options.delimiter))
        options.delimiter = [options.delimiter];
      if (options.delimiter.length === 0) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_DELIMITER",
          [
            "Invalid option delimiter:",
            "delimiter must be a non empty string or buffer or array of string|buffer,",
            `got ${delimiter_json}`
          ],
          options
        );
      }
      options.delimiter = options.delimiter.map(function(delimiter) {
        if (delimiter === void 0 || delimiter === null || delimiter === false) {
          return Buffer.from(",", options.encoding);
        }
        if (typeof delimiter === "string") {
          delimiter = Buffer.from(delimiter, options.encoding);
        }
        if (!Buffer.isBuffer(delimiter) || delimiter.length === 0) {
          throw new CsvError2(
            "CSV_INVALID_OPTION_DELIMITER",
            [
              "Invalid option delimiter:",
              "delimiter must be a non empty string or buffer or array of string|buffer,",
              `got ${delimiter_json}`
            ],
            options
          );
        }
        return delimiter;
      });
      if (options.escape === void 0 || options.escape === true) {
        options.escape = Buffer.from('"', options.encoding);
      } else if (typeof options.escape === "string") {
        options.escape = Buffer.from(options.escape, options.encoding);
      } else if (options.escape === null || options.escape === false) {
        options.escape = null;
      }
      if (options.escape !== null) {
        if (!Buffer.isBuffer(options.escape)) {
          throw new Error(
            `Invalid Option: escape must be a buffer, a string or a boolean, got ${JSON.stringify(options.escape)}`
          );
        }
      }
      if (options.from === void 0 || options.from === null) {
        options.from = 1;
      } else {
        if (typeof options.from === "string" && /\d+/.test(options.from)) {
          options.from = parseInt(options.from);
        }
        if (Number.isInteger(options.from)) {
          if (options.from < 0) {
            throw new Error(
              `Invalid Option: from must be a positive integer, got ${JSON.stringify(opts.from)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: from must be an integer, got ${JSON.stringify(options.from)}`
          );
        }
      }
      if (options.from_line === void 0 || options.from_line === null) {
        options.from_line = 1;
      } else {
        if (typeof options.from_line === "string" && /\d+/.test(options.from_line)) {
          options.from_line = parseInt(options.from_line);
        }
        if (Number.isInteger(options.from_line)) {
          if (options.from_line <= 0) {
            throw new Error(
              `Invalid Option: from_line must be a positive integer greater than 0, got ${JSON.stringify(opts.from_line)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: from_line must be an integer, got ${JSON.stringify(opts.from_line)}`
          );
        }
      }
      if (options.ignore_last_delimiters === void 0 || options.ignore_last_delimiters === null) {
        options.ignore_last_delimiters = false;
      } else if (typeof options.ignore_last_delimiters === "number") {
        options.ignore_last_delimiters = Math.floor(options.ignore_last_delimiters);
        if (options.ignore_last_delimiters === 0) {
          options.ignore_last_delimiters = false;
        }
      } else if (typeof options.ignore_last_delimiters !== "boolean") {
        throw new CsvError2(
          "CSV_INVALID_OPTION_IGNORE_LAST_DELIMITERS",
          [
            "Invalid option `ignore_last_delimiters`:",
            "the value must be a boolean value or an integer,",
            `got ${JSON.stringify(options.ignore_last_delimiters)}`
          ],
          options
        );
      }
      if (options.ignore_last_delimiters === true && options.columns === false) {
        throw new CsvError2(
          "CSV_IGNORE_LAST_DELIMITERS_REQUIRES_COLUMNS",
          [
            "The option `ignore_last_delimiters`",
            "requires the activation of the `columns` option"
          ],
          options
        );
      }
      if (options.info === void 0 || options.info === null || options.info === false) {
        options.info = false;
      } else if (options.info !== true) {
        throw new Error(
          `Invalid Option: info must be true, got ${JSON.stringify(options.info)}`
        );
      }
      if (options.max_record_size === void 0 || options.max_record_size === null || options.max_record_size === false) {
        options.max_record_size = 0;
      } else if (Number.isInteger(options.max_record_size) && options.max_record_size >= 0) {
      } else if (typeof options.max_record_size === "string" && /\d+/.test(options.max_record_size)) {
        options.max_record_size = parseInt(options.max_record_size);
      } else {
        throw new Error(
          `Invalid Option: max_record_size must be a positive integer, got ${JSON.stringify(options.max_record_size)}`
        );
      }
      if (options.objname === void 0 || options.objname === null || options.objname === false) {
        options.objname = void 0;
      } else if (Buffer.isBuffer(options.objname)) {
        if (options.objname.length === 0) {
          throw new Error(`Invalid Option: objname must be a non empty buffer`);
        }
        if (options.encoding === null) {
        } else {
          options.objname = options.objname.toString(options.encoding);
        }
      } else if (typeof options.objname === "string") {
        if (options.objname.length === 0) {
          throw new Error(`Invalid Option: objname must be a non empty string`);
        }
      } else if (typeof options.objname === "number") {
      } else {
        throw new Error(
          `Invalid Option: objname must be a string or a buffer, got ${options.objname}`
        );
      }
      if (options.objname !== void 0) {
        if (typeof options.objname === "number") {
          if (options.columns !== false) {
            throw Error(
              "Invalid Option: objname index cannot be combined with columns or be defined as a field"
            );
          }
        } else {
          if (options.columns === false) {
            throw Error(
              "Invalid Option: objname field must be combined with columns or be defined as an index"
            );
          }
        }
      }
      if (options.on_record === void 0 || options.on_record === null) {
        options.on_record = void 0;
      } else if (typeof options.on_record !== "function") {
        throw new CsvError2(
          "CSV_INVALID_OPTION_ON_RECORD",
          [
            "Invalid option `on_record`:",
            "expect a function,",
            `got ${JSON.stringify(options.on_record)}`
          ],
          options
        );
      }
      if (options.on_skip !== void 0 && options.on_skip !== null && typeof options.on_skip !== "function") {
        throw new Error(
          `Invalid Option: on_skip must be a function, got ${JSON.stringify(options.on_skip)}`
        );
      }
      if (options.quote === null || options.quote === false || options.quote === "") {
        options.quote = null;
      } else {
        if (options.quote === void 0 || options.quote === true) {
          options.quote = Buffer.from('"', options.encoding);
        } else if (typeof options.quote === "string") {
          options.quote = Buffer.from(options.quote, options.encoding);
        }
        if (!Buffer.isBuffer(options.quote)) {
          throw new Error(
            `Invalid Option: quote must be a buffer or a string, got ${JSON.stringify(options.quote)}`
          );
        }
      }
      if (options.raw === void 0 || options.raw === null || options.raw === false) {
        options.raw = false;
      } else if (options.raw !== true) {
        throw new Error(
          `Invalid Option: raw must be true, got ${JSON.stringify(options.raw)}`
        );
      }
      if (options.record_delimiter === void 0) {
        options.record_delimiter = [];
      } else if (typeof options.record_delimiter === "string" || Buffer.isBuffer(options.record_delimiter)) {
        if (options.record_delimiter.length === 0) {
          throw new CsvError2(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a non empty string or buffer,",
              `got ${JSON.stringify(options.record_delimiter)}`
            ],
            options
          );
        }
        options.record_delimiter = [options.record_delimiter];
      } else if (!Array.isArray(options.record_delimiter)) {
        throw new CsvError2(
          "CSV_INVALID_OPTION_RECORD_DELIMITER",
          [
            "Invalid option `record_delimiter`:",
            "value must be a string, a buffer or array of string|buffer,",
            `got ${JSON.stringify(options.record_delimiter)}`
          ],
          options
        );
      }
      options.record_delimiter = options.record_delimiter.map(function(rd, i) {
        if (typeof rd !== "string" && !Buffer.isBuffer(rd)) {
          throw new CsvError2(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a string, a buffer or array of string|buffer",
              `at index ${i},`,
              `got ${JSON.stringify(rd)}`
            ],
            options
          );
        } else if (rd.length === 0) {
          throw new CsvError2(
            "CSV_INVALID_OPTION_RECORD_DELIMITER",
            [
              "Invalid option `record_delimiter`:",
              "value must be a non empty string or buffer",
              `at index ${i},`,
              `got ${JSON.stringify(rd)}`
            ],
            options
          );
        }
        if (typeof rd === "string") {
          rd = Buffer.from(rd, options.encoding);
        }
        return rd;
      });
      if (typeof options.relax_column_count === "boolean") {
      } else if (options.relax_column_count === void 0 || options.relax_column_count === null) {
        options.relax_column_count = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count must be a boolean, got ${JSON.stringify(options.relax_column_count)}`
        );
      }
      if (typeof options.relax_column_count_less === "boolean") {
      } else if (options.relax_column_count_less === void 0 || options.relax_column_count_less === null) {
        options.relax_column_count_less = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count_less must be a boolean, got ${JSON.stringify(options.relax_column_count_less)}`
        );
      }
      if (typeof options.relax_column_count_more === "boolean") {
      } else if (options.relax_column_count_more === void 0 || options.relax_column_count_more === null) {
        options.relax_column_count_more = false;
      } else {
        throw new Error(
          `Invalid Option: relax_column_count_more must be a boolean, got ${JSON.stringify(options.relax_column_count_more)}`
        );
      }
      if (typeof options.relax_quotes === "boolean") {
      } else if (options.relax_quotes === void 0 || options.relax_quotes === null) {
        options.relax_quotes = false;
      } else {
        throw new Error(
          `Invalid Option: relax_quotes must be a boolean, got ${JSON.stringify(options.relax_quotes)}`
        );
      }
      if (typeof options.skip_empty_lines === "boolean") {
      } else if (options.skip_empty_lines === void 0 || options.skip_empty_lines === null) {
        options.skip_empty_lines = false;
      } else {
        throw new Error(
          `Invalid Option: skip_empty_lines must be a boolean, got ${JSON.stringify(options.skip_empty_lines)}`
        );
      }
      if (typeof options.skip_records_with_empty_values === "boolean") {
      } else if (options.skip_records_with_empty_values === void 0 || options.skip_records_with_empty_values === null) {
        options.skip_records_with_empty_values = false;
      } else {
        throw new Error(
          `Invalid Option: skip_records_with_empty_values must be a boolean, got ${JSON.stringify(options.skip_records_with_empty_values)}`
        );
      }
      if (typeof options.skip_records_with_error === "boolean") {
      } else if (options.skip_records_with_error === void 0 || options.skip_records_with_error === null) {
        options.skip_records_with_error = false;
      } else {
        throw new Error(
          `Invalid Option: skip_records_with_error must be a boolean, got ${JSON.stringify(options.skip_records_with_error)}`
        );
      }
      if (options.rtrim === void 0 || options.rtrim === null || options.rtrim === false) {
        options.rtrim = false;
      } else if (options.rtrim !== true) {
        throw new Error(
          `Invalid Option: rtrim must be a boolean, got ${JSON.stringify(options.rtrim)}`
        );
      }
      if (options.ltrim === void 0 || options.ltrim === null || options.ltrim === false) {
        options.ltrim = false;
      } else if (options.ltrim !== true) {
        throw new Error(
          `Invalid Option: ltrim must be a boolean, got ${JSON.stringify(options.ltrim)}`
        );
      }
      if (options.trim === void 0 || options.trim === null || options.trim === false) {
        options.trim = false;
      } else if (options.trim !== true) {
        throw new Error(
          `Invalid Option: trim must be a boolean, got ${JSON.stringify(options.trim)}`
        );
      }
      if (options.trim === true && opts.ltrim !== false) {
        options.ltrim = true;
      } else if (options.ltrim !== true) {
        options.ltrim = false;
      }
      if (options.trim === true && opts.rtrim !== false) {
        options.rtrim = true;
      } else if (options.rtrim !== true) {
        options.rtrim = false;
      }
      if (options.to === void 0 || options.to === null) {
        options.to = -1;
      } else if (options.to !== -1) {
        if (typeof options.to === "string" && /\d+/.test(options.to)) {
          options.to = parseInt(options.to);
        }
        if (Number.isInteger(options.to)) {
          if (options.to <= 0) {
            throw new Error(
              `Invalid Option: to must be a positive integer greater than 0, got ${JSON.stringify(opts.to)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: to must be an integer, got ${JSON.stringify(opts.to)}`
          );
        }
      }
      if (options.to_line === void 0 || options.to_line === null) {
        options.to_line = -1;
      } else if (options.to_line !== -1) {
        if (typeof options.to_line === "string" && /\d+/.test(options.to_line)) {
          options.to_line = parseInt(options.to_line);
        }
        if (Number.isInteger(options.to_line)) {
          if (options.to_line <= 0) {
            throw new Error(
              `Invalid Option: to_line must be a positive integer greater than 0, got ${JSON.stringify(opts.to_line)}`
            );
          }
        } else {
          throw new Error(
            `Invalid Option: to_line must be an integer, got ${JSON.stringify(opts.to_line)}`
          );
        }
      }
      return options;
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/index.js
var isRecordEmpty2, cr3, nl3, boms2, transform2;
var init_api = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/api/index.js"() {
    init_normalize_columns_array();
    init_init_state();
    init_normalize_options();
    init_CsvError();
    isRecordEmpty2 = function(record) {
      return record.every(
        (field) => field == null || field.toString && field.toString().trim() === ""
      );
    };
    cr3 = 13;
    nl3 = 10;
    boms2 = {
      // Note, the following are equals:
      // Buffer.from("\ufeff")
      // Buffer.from([239, 187, 191])
      // Buffer.from('EFBBBF', 'hex')
      utf8: Buffer.from([239, 187, 191]),
      // Note, the following are equals:
      // Buffer.from "\ufeff", 'utf16le
      // Buffer.from([255, 254])
      utf16le: Buffer.from([255, 254])
    };
    transform2 = function(original_options = {}) {
      const info2 = {
        bytes: 0,
        comment_lines: 0,
        empty_lines: 0,
        invalid_field_length: 0,
        lines: 1,
        records: 0
      };
      const options = normalize_options2(original_options);
      return {
        info: info2,
        original_options,
        options,
        state: init_state2(options),
        __needMoreData: function(i, bufLen, end) {
          if (end)
            return false;
          const { encoding, escape, quote } = this.options;
          const { quoting, needMoreDataSize, recordDelimiterMaxLength } = this.state;
          const numOfCharLeft = bufLen - i - 1;
          const requiredLength = Math.max(
            needMoreDataSize,
            // Skip if the remaining buffer smaller than record delimiter
            // If "record_delimiter" is yet to be discovered:
            // 1. It is equals to `[]` and "recordDelimiterMaxLength" equals `0`
            // 2. We set the length to windows line ending in the current encoding
            // Note, that encoding is known from user or bom discovery at that point
            // recordDelimiterMaxLength,
            recordDelimiterMaxLength === 0 ? Buffer.from("\r\n", encoding).length : recordDelimiterMaxLength,
            // Skip if remaining buffer can be an escaped quote
            quoting ? (escape === null ? 0 : escape.length) + quote.length : 0,
            // Skip if remaining buffer can be record delimiter following the closing quote
            quoting ? quote.length + recordDelimiterMaxLength : 0
          );
          return numOfCharLeft < requiredLength;
        },
        // Central parser implementation
        parse: function(nextBuf, end, push, close) {
          const {
            bom,
            comment_no_infix,
            encoding,
            from_line,
            ltrim,
            max_record_size,
            raw,
            relax_quotes,
            rtrim,
            skip_empty_lines,
            to,
            to_line
          } = this.options;
          let { comment, escape, quote, record_delimiter } = this.options;
          const { bomSkipped, previousBuf, rawBuffer, escapeIsQuote } = this.state;
          let buf;
          if (previousBuf === void 0) {
            if (nextBuf === void 0) {
              close();
              return;
            } else {
              buf = nextBuf;
            }
          } else if (previousBuf !== void 0 && nextBuf === void 0) {
            buf = previousBuf;
          } else {
            buf = Buffer.concat([previousBuf, nextBuf]);
          }
          if (bomSkipped === false) {
            if (bom === false) {
              this.state.bomSkipped = true;
            } else if (buf.length < 3) {
              if (end === false) {
                this.state.previousBuf = buf;
                return;
              }
            } else {
              for (const encoding2 in boms2) {
                if (boms2[encoding2].compare(buf, 0, boms2[encoding2].length) === 0) {
                  const bomLength = boms2[encoding2].length;
                  this.state.bufBytesStart += bomLength;
                  buf = buf.slice(bomLength);
                  const options2 = normalize_options2({
                    ...this.original_options,
                    encoding: encoding2
                  });
                  for (const key in options2) {
                    this.options[key] = options2[key];
                  }
                  ({ comment, escape, quote } = this.options);
                  break;
                }
              }
              this.state.bomSkipped = true;
            }
          }
          const bufLen = buf.length;
          let pos;
          for (pos = 0; pos < bufLen; pos++) {
            if (this.__needMoreData(pos, bufLen, end)) {
              break;
            }
            if (this.state.wasRowDelimiter === true) {
              this.info.lines++;
              this.state.wasRowDelimiter = false;
            }
            if (to_line !== -1 && this.info.lines > to_line) {
              this.state.stop = true;
              close();
              return;
            }
            if (this.state.quoting === false && record_delimiter.length === 0) {
              const record_delimiterCount = this.__autoDiscoverRecordDelimiter(
                buf,
                pos
              );
              if (record_delimiterCount) {
                record_delimiter = this.options.record_delimiter;
              }
            }
            const chr = buf[pos];
            if (raw === true) {
              rawBuffer.append(chr);
            }
            if ((chr === cr3 || chr === nl3) && this.state.wasRowDelimiter === false) {
              this.state.wasRowDelimiter = true;
            }
            if (this.state.escaping === true) {
              this.state.escaping = false;
            } else {
              if (escape !== null && this.state.quoting === true && this.__isEscape(buf, pos, chr) && pos + escape.length < bufLen) {
                if (escapeIsQuote) {
                  if (this.__isQuote(buf, pos + escape.length)) {
                    this.state.escaping = true;
                    pos += escape.length - 1;
                    continue;
                  }
                } else {
                  this.state.escaping = true;
                  pos += escape.length - 1;
                  continue;
                }
              }
              if (this.state.commenting === false && this.__isQuote(buf, pos)) {
                if (this.state.quoting === true) {
                  const nextChr = buf[pos + quote.length];
                  const isNextChrTrimable = rtrim && this.__isCharTrimable(buf, pos + quote.length);
                  const isNextChrComment = comment !== null && this.__compareBytes(comment, buf, pos + quote.length, nextChr);
                  const isNextChrDelimiter = this.__isDelimiter(
                    buf,
                    pos + quote.length,
                    nextChr
                  );
                  const isNextChrRecordDelimiter = record_delimiter.length === 0 ? this.__autoDiscoverRecordDelimiter(buf, pos + quote.length) : this.__isRecordDelimiter(nextChr, buf, pos + quote.length);
                  if (escape !== null && this.__isEscape(buf, pos, chr) && this.__isQuote(buf, pos + escape.length)) {
                    pos += escape.length - 1;
                  } else if (!nextChr || isNextChrDelimiter || isNextChrRecordDelimiter || isNextChrComment || isNextChrTrimable) {
                    this.state.quoting = false;
                    this.state.wasQuoting = true;
                    pos += quote.length - 1;
                    continue;
                  } else if (relax_quotes === false) {
                    const err = this.__error(
                      new CsvError2(
                        "CSV_INVALID_CLOSING_QUOTE",
                        [
                          "Invalid Closing Quote:",
                          `got "${String.fromCharCode(nextChr)}"`,
                          `at line ${this.info.lines}`,
                          "instead of delimiter, record delimiter, trimable character",
                          "(if activated) or comment"
                        ],
                        this.options,
                        this.__infoField()
                      )
                    );
                    if (err !== void 0)
                      return err;
                  } else {
                    this.state.quoting = false;
                    this.state.wasQuoting = true;
                    this.state.field.prepend(quote);
                    pos += quote.length - 1;
                  }
                } else {
                  if (this.state.field.length !== 0) {
                    if (relax_quotes === false) {
                      const info3 = this.__infoField();
                      const bom2 = Object.keys(boms2).map(
                        (b) => boms2[b].equals(this.state.field.toString()) ? b : false
                      ).filter(Boolean)[0];
                      const err = this.__error(
                        new CsvError2(
                          "INVALID_OPENING_QUOTE",
                          [
                            "Invalid Opening Quote:",
                            `a quote is found on field ${JSON.stringify(info3.column)} at line ${info3.lines}, value is ${JSON.stringify(this.state.field.toString(encoding))}`,
                            bom2 ? `(${bom2} bom)` : void 0
                          ],
                          this.options,
                          info3,
                          {
                            field: this.state.field
                          }
                        )
                      );
                      if (err !== void 0)
                        return err;
                    }
                  } else {
                    this.state.quoting = true;
                    pos += quote.length - 1;
                    continue;
                  }
                }
              }
              if (this.state.quoting === false) {
                const recordDelimiterLength = this.__isRecordDelimiter(
                  chr,
                  buf,
                  pos
                );
                if (recordDelimiterLength !== 0) {
                  const skipCommentLine = this.state.commenting && this.state.wasQuoting === false && this.state.record.length === 0 && this.state.field.length === 0;
                  if (skipCommentLine) {
                    this.info.comment_lines++;
                  } else {
                    if (this.state.enabled === false && this.info.lines + (this.state.wasRowDelimiter === true ? 1 : 0) >= from_line) {
                      this.state.enabled = true;
                      this.__resetField();
                      this.__resetRecord();
                      pos += recordDelimiterLength - 1;
                      continue;
                    }
                    if (skip_empty_lines === true && this.state.wasQuoting === false && this.state.record.length === 0 && this.state.field.length === 0) {
                      this.info.empty_lines++;
                      pos += recordDelimiterLength - 1;
                      continue;
                    }
                    this.info.bytes = this.state.bufBytesStart + pos;
                    const errField = this.__onField();
                    if (errField !== void 0)
                      return errField;
                    this.info.bytes = this.state.bufBytesStart + pos + recordDelimiterLength;
                    const errRecord = this.__onRecord(push);
                    if (errRecord !== void 0)
                      return errRecord;
                    if (to !== -1 && this.info.records >= to) {
                      this.state.stop = true;
                      close();
                      return;
                    }
                  }
                  this.state.commenting = false;
                  pos += recordDelimiterLength - 1;
                  continue;
                }
                if (this.state.commenting) {
                  continue;
                }
                if (comment !== null && (comment_no_infix === false || this.state.record.length === 0 && this.state.field.length === 0)) {
                  const commentCount = this.__compareBytes(comment, buf, pos, chr);
                  if (commentCount !== 0) {
                    this.state.commenting = true;
                    continue;
                  }
                }
                const delimiterLength = this.__isDelimiter(buf, pos, chr);
                if (delimiterLength !== 0) {
                  this.info.bytes = this.state.bufBytesStart + pos;
                  const errField = this.__onField();
                  if (errField !== void 0)
                    return errField;
                  pos += delimiterLength - 1;
                  continue;
                }
              }
            }
            if (this.state.commenting === false) {
              if (max_record_size !== 0 && this.state.record_length + this.state.field.length > max_record_size) {
                return this.__error(
                  new CsvError2(
                    "CSV_MAX_RECORD_SIZE",
                    [
                      "Max Record Size:",
                      "record exceed the maximum number of tolerated bytes",
                      `of ${max_record_size}`,
                      `at line ${this.info.lines}`
                    ],
                    this.options,
                    this.__infoField()
                  )
                );
              }
            }
            const lappend = ltrim === false || this.state.quoting === true || this.state.field.length !== 0 || !this.__isCharTrimable(buf, pos);
            const rappend = rtrim === false || this.state.wasQuoting === false;
            if (lappend === true && rappend === true) {
              this.state.field.append(chr);
            } else if (rtrim === true && !this.__isCharTrimable(buf, pos)) {
              return this.__error(
                new CsvError2(
                  "CSV_NON_TRIMABLE_CHAR_AFTER_CLOSING_QUOTE",
                  [
                    "Invalid Closing Quote:",
                    "found non trimable byte after quote",
                    `at line ${this.info.lines}`
                  ],
                  this.options,
                  this.__infoField()
                )
              );
            } else {
              if (lappend === false) {
                pos += this.__isCharTrimable(buf, pos) - 1;
              }
              continue;
            }
          }
          if (end === true) {
            if (this.state.quoting === true) {
              const err = this.__error(
                new CsvError2(
                  "CSV_QUOTE_NOT_CLOSED",
                  [
                    "Quote Not Closed:",
                    `the parsing is finished with an opening quote at line ${this.info.lines}`
                  ],
                  this.options,
                  this.__infoField()
                )
              );
              if (err !== void 0)
                return err;
            } else {
              if (this.state.wasQuoting === true || this.state.record.length !== 0 || this.state.field.length !== 0) {
                this.info.bytes = this.state.bufBytesStart + pos;
                const errField = this.__onField();
                if (errField !== void 0)
                  return errField;
                const errRecord = this.__onRecord(push);
                if (errRecord !== void 0)
                  return errRecord;
              } else if (this.state.wasRowDelimiter === true) {
                this.info.empty_lines++;
              } else if (this.state.commenting === true) {
                this.info.comment_lines++;
              }
            }
          } else {
            this.state.bufBytesStart += pos;
            this.state.previousBuf = buf.slice(pos);
          }
          if (this.state.wasRowDelimiter === true) {
            this.info.lines++;
            this.state.wasRowDelimiter = false;
          }
        },
        __onRecord: function(push) {
          const {
            columns,
            group_columns_by_name,
            encoding,
            info: info3,
            from: from2,
            relax_column_count,
            relax_column_count_less,
            relax_column_count_more,
            raw,
            skip_records_with_empty_values
          } = this.options;
          const { enabled, record } = this.state;
          if (enabled === false) {
            return this.__resetRecord();
          }
          const recordLength = record.length;
          if (columns === true) {
            if (skip_records_with_empty_values === true && isRecordEmpty2(record)) {
              this.__resetRecord();
              return;
            }
            return this.__firstLineToColumns(record);
          }
          if (columns === false && this.info.records === 0) {
            this.state.expectedRecordLength = recordLength;
          }
          if (recordLength !== this.state.expectedRecordLength) {
            const err = columns === false ? new CsvError2(
              "CSV_RECORD_INCONSISTENT_FIELDS_LENGTH",
              [
                "Invalid Record Length:",
                `expect ${this.state.expectedRecordLength},`,
                `got ${recordLength} on line ${this.info.lines}`
              ],
              this.options,
              this.__infoField(),
              {
                record
              }
            ) : new CsvError2(
              "CSV_RECORD_INCONSISTENT_COLUMNS",
              [
                "Invalid Record Length:",
                `columns length is ${columns.length},`,
                // rename columns
                `got ${recordLength} on line ${this.info.lines}`
              ],
              this.options,
              this.__infoField(),
              {
                record
              }
            );
            if (relax_column_count === true || relax_column_count_less === true && recordLength < this.state.expectedRecordLength || relax_column_count_more === true && recordLength > this.state.expectedRecordLength) {
              this.info.invalid_field_length++;
              this.state.error = err;
            } else {
              const finalErr = this.__error(err);
              if (finalErr)
                return finalErr;
            }
          }
          if (skip_records_with_empty_values === true && isRecordEmpty2(record)) {
            this.__resetRecord();
            return;
          }
          if (this.state.recordHasError === true) {
            this.__resetRecord();
            this.state.recordHasError = false;
            return;
          }
          this.info.records++;
          if (from2 === 1 || this.info.records >= from2) {
            const { objname } = this.options;
            if (columns !== false) {
              const obj = {};
              for (let i = 0, l = record.length; i < l; i++) {
                if (columns[i] === void 0 || columns[i].disabled)
                  continue;
                if (group_columns_by_name === true && obj[columns[i].name] !== void 0) {
                  if (Array.isArray(obj[columns[i].name])) {
                    obj[columns[i].name] = obj[columns[i].name].concat(record[i]);
                  } else {
                    obj[columns[i].name] = [obj[columns[i].name], record[i]];
                  }
                } else {
                  obj[columns[i].name] = record[i];
                }
              }
              if (raw === true || info3 === true) {
                const extRecord = Object.assign(
                  { record: obj },
                  raw === true ? { raw: this.state.rawBuffer.toString(encoding) } : {},
                  info3 === true ? { info: this.__infoRecord() } : {}
                );
                const err = this.__push(
                  objname === void 0 ? extRecord : [obj[objname], extRecord],
                  push
                );
                if (err) {
                  return err;
                }
              } else {
                const err = this.__push(
                  objname === void 0 ? obj : [obj[objname], obj],
                  push
                );
                if (err) {
                  return err;
                }
              }
            } else {
              if (raw === true || info3 === true) {
                const extRecord = Object.assign(
                  { record },
                  raw === true ? { raw: this.state.rawBuffer.toString(encoding) } : {},
                  info3 === true ? { info: this.__infoRecord() } : {}
                );
                const err = this.__push(
                  objname === void 0 ? extRecord : [record[objname], extRecord],
                  push
                );
                if (err) {
                  return err;
                }
              } else {
                const err = this.__push(
                  objname === void 0 ? record : [record[objname], record],
                  push
                );
                if (err) {
                  return err;
                }
              }
            }
          }
          this.__resetRecord();
        },
        __firstLineToColumns: function(record) {
          const { firstLineToHeaders } = this.state;
          try {
            const headers = firstLineToHeaders === void 0 ? record : firstLineToHeaders.call(null, record);
            if (!Array.isArray(headers)) {
              return this.__error(
                new CsvError2(
                  "CSV_INVALID_COLUMN_MAPPING",
                  [
                    "Invalid Column Mapping:",
                    "expect an array from column function,",
                    `got ${JSON.stringify(headers)}`
                  ],
                  this.options,
                  this.__infoField(),
                  {
                    headers
                  }
                )
              );
            }
            const normalizedHeaders = normalize_columns_array2(headers);
            this.state.expectedRecordLength = normalizedHeaders.length;
            this.options.columns = normalizedHeaders;
            this.__resetRecord();
            return;
          } catch (err) {
            return err;
          }
        },
        __resetRecord: function() {
          if (this.options.raw === true) {
            this.state.rawBuffer.reset();
          }
          this.state.error = void 0;
          this.state.record = [];
          this.state.record_length = 0;
        },
        __onField: function() {
          const { cast, encoding, rtrim, max_record_size } = this.options;
          const { enabled, wasQuoting } = this.state;
          if (enabled === false) {
            return this.__resetField();
          }
          let field = this.state.field.toString(encoding);
          if (rtrim === true && wasQuoting === false) {
            field = field.trimRight();
          }
          if (cast === true) {
            const [err, f] = this.__cast(field);
            if (err !== void 0)
              return err;
            field = f;
          }
          this.state.record.push(field);
          if (max_record_size !== 0 && typeof field === "string") {
            this.state.record_length += field.length;
          }
          this.__resetField();
        },
        __resetField: function() {
          this.state.field.reset();
          this.state.wasQuoting = false;
        },
        __push: function(record, push) {
          const { on_record } = this.options;
          if (on_record !== void 0) {
            const info3 = this.__infoRecord();
            try {
              record = on_record.call(null, record, info3);
            } catch (err) {
              return err;
            }
            if (record === void 0 || record === null) {
              return;
            }
          }
          push(record);
        },
        // Return a tuple with the error and the casted value
        __cast: function(field) {
          const { columns, relax_column_count } = this.options;
          const isColumns = Array.isArray(columns);
          if (isColumns === true && relax_column_count && this.options.columns.length <= this.state.record.length) {
            return [void 0, void 0];
          }
          if (this.state.castField !== null) {
            try {
              const info3 = this.__infoField();
              return [void 0, this.state.castField.call(null, field, info3)];
            } catch (err) {
              return [err];
            }
          }
          if (this.__isFloat(field)) {
            return [void 0, parseFloat(field)];
          } else if (this.options.cast_date !== false) {
            const info3 = this.__infoField();
            return [void 0, this.options.cast_date.call(null, field, info3)];
          }
          return [void 0, field];
        },
        // Helper to test if a character is a space or a line delimiter
        __isCharTrimable: function(buf, pos) {
          const isTrim = (buf2, pos2) => {
            const { timchars } = this.state;
            loop1:
              for (let i = 0; i < timchars.length; i++) {
                const timchar = timchars[i];
                for (let j = 0; j < timchar.length; j++) {
                  if (timchar[j] !== buf2[pos2 + j])
                    continue loop1;
                }
                return timchar.length;
              }
            return 0;
          };
          return isTrim(buf, pos);
        },
        // Keep it in case we implement the `cast_int` option
        // __isInt(value){
        //   // return Number.isInteger(parseInt(value))
        //   // return !isNaN( parseInt( obj ) );
        //   return /^(\-|\+)?[1-9][0-9]*$/.test(value)
        // }
        __isFloat: function(value) {
          return value - parseFloat(value) + 1 >= 0;
        },
        __compareBytes: function(sourceBuf, targetBuf, targetPos, firstByte) {
          if (sourceBuf[0] !== firstByte)
            return 0;
          const sourceLength = sourceBuf.length;
          for (let i = 1; i < sourceLength; i++) {
            if (sourceBuf[i] !== targetBuf[targetPos + i])
              return 0;
          }
          return sourceLength;
        },
        __isDelimiter: function(buf, pos, chr) {
          const { delimiter, ignore_last_delimiters } = this.options;
          if (ignore_last_delimiters === true && this.state.record.length === this.options.columns.length - 1) {
            return 0;
          } else if (ignore_last_delimiters !== false && typeof ignore_last_delimiters === "number" && this.state.record.length === ignore_last_delimiters - 1) {
            return 0;
          }
          loop1:
            for (let i = 0; i < delimiter.length; i++) {
              const del = delimiter[i];
              if (del[0] === chr) {
                for (let j = 1; j < del.length; j++) {
                  if (del[j] !== buf[pos + j])
                    continue loop1;
                }
                return del.length;
              }
            }
          return 0;
        },
        __isRecordDelimiter: function(chr, buf, pos) {
          const { record_delimiter } = this.options;
          const recordDelimiterLength = record_delimiter.length;
          loop1:
            for (let i = 0; i < recordDelimiterLength; i++) {
              const rd = record_delimiter[i];
              const rdLength = rd.length;
              if (rd[0] !== chr) {
                continue;
              }
              for (let j = 1; j < rdLength; j++) {
                if (rd[j] !== buf[pos + j]) {
                  continue loop1;
                }
              }
              return rd.length;
            }
          return 0;
        },
        __isEscape: function(buf, pos, chr) {
          const { escape } = this.options;
          if (escape === null)
            return false;
          const l = escape.length;
          if (escape[0] === chr) {
            for (let i = 0; i < l; i++) {
              if (escape[i] !== buf[pos + i]) {
                return false;
              }
            }
            return true;
          }
          return false;
        },
        __isQuote: function(buf, pos) {
          const { quote } = this.options;
          if (quote === null)
            return false;
          const l = quote.length;
          for (let i = 0; i < l; i++) {
            if (quote[i] !== buf[pos + i]) {
              return false;
            }
          }
          return true;
        },
        __autoDiscoverRecordDelimiter: function(buf, pos) {
          const { encoding } = this.options;
          const rds = [
            // Important, the windows line ending must be before mac os 9
            Buffer.from("\r\n", encoding),
            Buffer.from("\n", encoding),
            Buffer.from("\r", encoding)
          ];
          loop:
            for (let i = 0; i < rds.length; i++) {
              const l = rds[i].length;
              for (let j = 0; j < l; j++) {
                if (rds[i][j] !== buf[pos + j]) {
                  continue loop;
                }
              }
              this.options.record_delimiter.push(rds[i]);
              this.state.recordDelimiterMaxLength = rds[i].length;
              return rds[i].length;
            }
          return 0;
        },
        __error: function(msg) {
          const { encoding, raw, skip_records_with_error } = this.options;
          const err = typeof msg === "string" ? new Error(msg) : msg;
          if (skip_records_with_error) {
            this.state.recordHasError = true;
            if (this.options.on_skip !== void 0) {
              try {
                this.options.on_skip(
                  err,
                  raw ? this.state.rawBuffer.toString(encoding) : void 0
                );
              } catch (err2) {
                return err2;
              }
            }
            return void 0;
          } else {
            return err;
          }
        },
        __infoDataSet: function() {
          return {
            ...this.info,
            columns: this.options.columns
          };
        },
        __infoRecord: function() {
          const { columns, raw, encoding } = this.options;
          return {
            ...this.__infoDataSet(),
            error: this.state.error,
            header: columns === true,
            index: this.state.record.length,
            raw: raw ? this.state.rawBuffer.toString(encoding) : void 0
          };
        },
        __infoField: function() {
          const { columns } = this.options;
          const isColumns = Array.isArray(columns);
          return {
            ...this.__infoRecord(),
            column: isColumns === true ? columns.length > this.state.record.length ? columns[this.state.record.length].name : null : this.state.record.length,
            quoting: this.state.wasQuoting
          };
        }
      };
    };
  }
});

// node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/sync.js
var sync_exports2 = {};
__export(sync_exports2, {
  CsvError: () => CsvError2,
  parse: () => parse2
});
var parse2;
var init_sync2 = __esm({
  "node_modules/.deno/csv-parse@6.1.0/node_modules/csv-parse/lib/sync.js"() {
    init_api();
    parse2 = function(data, opts = {}) {
      if (typeof data === "string") {
        data = Buffer.from(data);
      }
      const records = opts && opts.objname ? {} : [];
      const parser = transform2(opts);
      const push = (record) => {
        if (parser.options.objname === void 0)
          records.push(record);
        else {
          records[record[0]] = record[1];
        }
      };
      const close = () => {
      };
      const error2 = parser.parse(data, true, push, close);
      if (error2 !== void 0)
        throw error2;
      return records;
    };
  }
});

// src/validators/internal/emptyFile.ts
var emptyFile = (_schema, context) => {
  if (context.file.size === 0) {
    context.issues.addSchemaIssue("FileEmpty", [context.file]);
  }
  return Promise.resolve();
};

// src/validators/filenameIdentify.ts
var CHECKS = [
  findRuleMatches
];
async function filenameIdentify(schema, context) {
  for (const check of CHECKS) {
    await check(schema, context);
  }
}
function checkDirRules(schema, rulesRecord, baseDirs) {
  Object.keys(rulesRecord).filter((key) => {
    return key.startsWith("rules.files.common.core") && !rulesRecord[key];
  }).map((key) => {
    const node = schema[key];
    if (node.directory === true && baseDirs.includes(node.path)) {
      rulesRecord[key] = true;
    }
  });
}
function findFileRules(schema, rulesRecord) {
  const schemaPath = "rules.files";
  Object.keys(schema[schemaPath]).map((key) => {
    const path2 = `${schemaPath}.${key}`;
    _findFileRules(schema[path2], path2, rulesRecord);
  });
  return Promise.resolve();
}
function _findFileRules(node, path2, rulesRecord) {
  if ("baseDir" in node && "extensions" in node && ("suffix" in node || "stem" in node)) {
    rulesRecord[path2] = false;
    return;
  }
  if ("path" in node && "directory" in node) {
    rulesRecord[path2] = false;
    return;
  } else {
    Object.keys(node).map((key) => {
      if (typeof node[key] === "object") {
        _findFileRules(node[key], `${path2}.${key}`, rulesRecord);
      }
    });
  }
}
function findRuleMatches(schema, context) {
  const schemaPath = "rules.files";
  Object.keys(schema[schemaPath]).map((key) => {
    const path2 = `${schemaPath}.${key}`;
    _findRuleMatches(schema[path2], path2, context);
  });
  if (context.filenameRules.length === 0 && context.file.path !== "/.bidsignore") {
    context.issues.addSchemaIssue("FileNotChecked", [context.file]);
    if (context.file.name === "dataset_description.json") {
      context.issues.addSchemaIssue(
        "WrongMetadataLocation",
        [context.file],
        `You have placed a file called "dataset_description.json" within the ${context.baseDir} 
        subDirectory. Such files are only valid when placed in the root directory.`
      );
    }
  }
  return Promise.resolve();
}
function checkFileRules(arbitraryNesting, hasSuffix, node, context) {
  let baseDirCond = null;
  let suffixStemCond = null;
  if (arbitraryNesting) {
    baseDirCond = context.baseDir === node.baseDir;
  } else {
    if (context.baseDir === "/") {
      baseDirCond = context.path === `/${context.file.name}`;
    } else {
      baseDirCond = context.path === `/${node.baseDir}/${context.file.name}`;
    }
  }
  if (hasSuffix) {
    suffixStemCond = context.suffix === node.suffix;
  } else {
    suffixStemCond = context.file.name.startsWith(node.stem);
  }
  return baseDirCond && node.extensions.includes(context.extension) && suffixStemCond;
}
function _findRuleMatches(node, path2, context) {
  if ("arbitraryNesting" in node) {
    if (checkFileRules(node.arbitraryNesting, "suffix" in node, node, context)) {
      context.filenameRules.push(path2);
      return;
    }
  } else {
    Object.keys(node).map((key) => {
      if (typeof node[key] === "object") {
        _findRuleMatches(node[key], `${path2}.${key}`, context);
      }
    });
  }
}

// src/types/issues.ts
var Issue = class {
  constructor({
    key,
    severity,
    reason,
    requires,
    files
  }) {
    this.key = key;
    this.severity = severity;
    this.reason = reason;
    this.requires = requires;
    if (Array.isArray(files)) {
      this.files = /* @__PURE__ */ new Map();
      for (const f of files) {
        this.files.set(f.path, f);
      }
    } else {
      this.files = files;
    }
  }
  get helpUrl() {
    return `https://neurostars.org/search?q=${this.key}`;
  }
};

// src/issues/datasetIssues.ts
var CODE_DEPRECATED = Number.MIN_SAFE_INTEGER;
var issueFile = (issue, f) => {
  const evidence = f.evidence || "";
  const reason = issue.reason || "";
  const line = f.line || 0;
  const character = f.character || 0;
  return {
    key: issue.key,
    code: CODE_DEPRECATED,
    file: { path: f.path, name: f.name, relativePath: f.path },
    evidence,
    line,
    character,
    severity: issue.severity,
    reason,
    helpUrl: issue.helpUrl
  };
};
var DatasetIssues = class extends Map {
  /**
   * Creates a new dataset issues collection
   * @param schema - Optional schema containing issue definitions
   */
  constructor(schema) {
    super();
    this.schema = schema ? schema : {};
  }
  /**
   * Adds a new issue or updates an existing one
   * If an issue with the same key exists, merges the file references
   *
   * @param params - Issue parameters
   * @returns The added or updated issue
   */
  add({
    key,
    reason,
    severity = "error",
    requires = [],
    files = []
  }) {
    const existingIssue = this.get(key);
    if (existingIssue) {
      for (const f of files) {
        existingIssue.files.set(f.path, f);
      }
      return existingIssue;
    } else {
      const newIssue = new Issue({
        key,
        severity,
        reason,
        requires,
        files
      });
      this.set(key, newIssue);
      return newIssue;
    }
  }
  /**
   * Checks if a specific issue exists
   * @param key - Issue identifier to check
   * @returns True if the issue exists
   */
  hasIssue({ key }) {
    if (this.has(key)) {
      return true;
    }
    return false;
  }
  /**
   * Adds an issue using metadata from the schema
   * Retrieves issue details from schema's error definitions
   *
   * @param key - Schema error key
   * @param files - Array of affected files
   */
  addSchemaIssue(key, files) {
    if (this.schema) {
      this.add({
        key: this.schema[`rules.errors.${key}.code`],
        reason: this.schema[`rules.errors.${key}.reason`],
        severity: this.schema[`rules.errors.${key}.level`],
        requires: this.schema[`rules.errors.${key}.requires`],
        files
      });
    }
  }
  /**
   * Finds all issues affecting a specific file
   * @param path - File path to check
   * @returns Array of issues affecting the file
   */
  fileInIssues(path2) {
    const matchingIssues = [];
    for (const [_, issue] of this) {
      if (issue.files.get(path2)) {
        matchingIssues.push(issue);
      }
    }
    return matchingIssues;
  }
  /**
   * Gets issue keys for issues affecting a file
   * @param path - File path relative to dataset root
   * @returns Array of issue keys
   */
  getFileIssueKeys(path2) {
    return this.fileInIssues(path2).map((issue) => issue.key);
  }
  /**
   * Removes issues for objects that weren't found
   * Filters based on rule satisfaction record
   *
   * @param rulesRecord - Record of which rules were satisfied
   */
  filterIssues(rulesRecord) {
    for (const [_, issue] of this) {
      if (!issue.requires.every((req) => rulesRecord[req])) {
        this.delete(_);
      }
    }
  }
  /**
   * Formats issues for output
   * Converts internal representation to standardized output format
   * Separates issues by severity (errors vs warnings)
   *
   * @returns Formatted issues object with separate error and warning arrays
   */
  formatOutput() {
    const output = {
      errors: [],
      warnings: []
    };
    for (const [_, issue] of this) {
      const outputIssue = {
        severity: issue.severity,
        key: issue.key,
        code: CODE_DEPRECATED,
        additionalFileCount: 0,
        reason: issue.reason,
        files: Array.from(issue.files.values()).map((f) => issueFile(issue, f)),
        helpUrl: issue.helpUrl
      };
      if (issue.severity === "warning") {
        output.warnings.push(outputIssue);
      } else {
        output.errors.push(outputIssue);
      }
    }
    return output;
  }
};

// src/utils/platform.ts
//import process from "node:process";
var isBrowser = typeof window !== "undefined" && // deno-lint-ignore no-explicit-any
typeof window.document !== "undefined";
var isNode = typeof process !== "undefined" && process.versions != null && process.versions.node != null;
var isDeno = typeof Deno !== "undefined";
var _path;
var path = {
  /** Resolves a sequence of paths into an absolute path */
  resolve: (...paths) => _path ? _path.resolve(...paths) : paths.join("/"),
  /** Gets the last portion of a path */
  basename: (path2, ext) => _path ? _path.basename(path2, ext) : path2.split("/").pop() || "",
  /** Joins all given path segments together */
  join: (...paths) => _path ? _path.join(...paths) : paths.join("/"),
  /** Returns the directory name of a path */
  dirname: (path2) => _path ? _path.dirname(path2) : path2.split("/").slice(0, -1).join("/"),
  /** Platform-independent path separator */
  sep: "/"
};
var browserLogger = {
  info: console.info.bind(console),
  warn: console.warn.bind(console),
  error: console.error.bind(console),
  debug: console.debug.bind(console)
};
var readFile = async (filePath) => {
  if (isBrowser) {
    const response = await fetch(filePath);
    return response.text();
  } else if (isNode) {
    const fs = await import("node:fs/promises");
    return fs.readFile(filePath, "utf-8");
  } else {
    return Deno.readTextFile(filePath);
  }
};
var createReadStream = (filePath) => {
  if (isNode) {
    return new ReadableStream({
      async start(controller) {
        const fs = await import("node:fs");
        const readStream = fs.createReadStream(filePath);
        readStream.on("data", (chunk) => {
          if (chunk instanceof Uint8Array) {
            controller.enqueue(chunk);
          } else if (typeof chunk === "string") {
            controller.enqueue(new TextEncoder().encode(chunk));
          } else {
            controller.enqueue(new TextEncoder().encode(String(chunk)));
          }
        });
        readStream.on("end", () => controller.close());
        readStream.on("error", (error2) => controller.error(error2));
      }
    });
  } else if (isDeno) {
    const file = Deno.openSync(filePath, { read: true });
    return file.readable;
  } else {
    return new ReadableStream({
      async start(controller) {
        try {
          const response = await fetch(filePath);
          if (!response.body) {
            throw new Error("No readable body in response");
          }
          const reader = response.body.getReader();
          while (true) {
            const { done, value } = await reader.read();
            if (done)
              break;
            controller.enqueue(value);
          }
          controller.close();
        } catch (error2) {
          controller.error(error2);
        }
      }
    });
  }
};

// src/validators/filenameValidate.ts
var CHECKS2 = [
  checkRules
];
async function filenameValidate(schema, context) {
  for (const check of CHECKS2) {
    await check(schema, context);
  }
  return Promise.resolve();
}
var ruleChecks = [
  extensionMismatch,
  keywordCheck
];
function checkRules(schema, context) {
  if (context.filenameRules.length === 1) {
    for (const check of ruleChecks) {
      check(
        context.filenameRules[0],
        schema,
        context
      );
    }
  } else {
    const ogIssues = context.issues;
    const noIssues = [];
    const someIssues = [];
    for (const path2 of context.filenameRules) {
      const tempIssues = new DatasetIssues();
      context.issues = tempIssues;
      for (const check of ruleChecks) {
        check(path2, schema, context);
      }
      tempIssues.size ? someIssues.push([path2, tempIssues]) : noIssues.push([path2, tempIssues]);
    }
    if (noIssues.length) {
      context.issues = ogIssues;
      context.filenameRules = [noIssues[0][0]];
    } else if (someIssues.length) {
      context.issues = ogIssues;
      context.issues.addSchemaIssue("AllFilenameRulesHaveIssues", [
        {
          ...context.file,
          evidence: `Rules that matched with issues: ${someIssues.map((x) => x[0]).join(", ")}`
        }
      ]);
    }
  }
  return Promise.resolve();
}
function extensionMismatch(path2, schema, context) {
  const rule = schema[path2];
  if (Array.isArray(rule.extensions) && !rule.extensions.includes(context.extension)) {
    context.issues.addSchemaIssue("ExtensionMismatch", [
      { ...context.file, evidence: `Rule: ${path2}` }
    ]);
  }
}
function keywordCheck(path2, schema, context) {
  const rule = schema[path2];
  if ("usesKeywords" in rule && rule.usesKeywords) {
    if ("fileRegex" in rule) {
      const fileRegex = new RegExp(rule.fileRegex);
      const regexMatch = context.file.name.match(fileRegex);
      if (regexMatch && regexMatch[0] !== context.file.name || !regexMatch) {
        context.issues.addSchemaIssue(
          "FilenameKeywordFormattingError",
          [context.file]
        );
      }
    }
    if (!Object.keys(context.keywords).every(
      (keyword) => keyword in schema["meta.context.context.properties.keywords.properties"]
    )) {
      context.issues.addSchemaIssue(
        "FilenameUnofficialKeywordWarning",
        [context.file]
      );
    }
  }
}
function checkMissingRules(schema, rulesRecord, issues) {
  Object.keys(rulesRecord).filter((key) => {
    return rulesRecord[key] === false;
  }).map((key) => {
    const node = schema[key];
    issues.add({
      key: node.code,
      reason: node.reason,
      severity: node.level
    });
  });
}

// src/utils/memoize.ts
var memoize = (fn) => {
  const cache = /* @__PURE__ */ new Map();
  const cached = function(val) {
    return cache.has(val) ? cache.get(val) : cache.set(val, fn.call(this, val)) && cache.get(val);
  };
  cached.cache = cache;
  return cached;
};

// src/schema/applyRules.ts
function applyRules(schema, context, rootSchema, schemaPath) {
  if (!rootSchema) {
    rootSchema = schema;
  }
  if (!schemaPath) {
    schemaPath = "schema";
  }
  for (const key in schema) {
    if (!(schema[key].constructor === Object)) {
      continue;
    }
    if ("selectors" in schema[key]) {
      evalRule(
        schema[key],
        context,
        rootSchema,
        `${schemaPath}.${key}`
      );
    } else if (schema[key].constructor === Object) {
      applyRules(
        schema[key],
        context,
        rootSchema,
        `${schemaPath}.${key}`
      );
    }
  }
  return Promise.resolve();
}
var evalConstructor = (src) => new Function("context", `with (context) { return ${src} }`);
var safeHas = () => true;
var safeGet = (target, prop) => prop === Symbol.unscopables ? void 0 : target[prop];
var memoizedEvalConstructor = memoize(evalConstructor);
function evalCheck(src, context) {
  const test = memoizedEvalConstructor(src);
  const safeContext = new Proxy(context, { has: safeHas, get: safeGet });
  try {
    return test(safeContext);
  } catch (_error) {
    return false;
  }
}
var evalMap = {
  columnsMatchMetadata: evalColumns,
  fields: evalJsonCheck
};
function evalRule(rule, context, schema, schemaPath) {
  if (rule.selectors && !mapEvalCheck(rule.selectors, context)) {
    return;
  }
  Object.keys(rule).filter((key) => key in evalMap).map((key) => {
    evalMap[key](rule, context, schema, schemaPath);
  });
}
function mapEvalCheck(statements, context) {
  return statements.every((x) => evalCheck(x, context));
}
function evalColumns(_rule, context, schema, schemaPath) {
  if (context.extension !== ".csv")
    return;
  const headers = [...Object.keys(context.columns)];
  context.dataset.allColumns = [.../* @__PURE__ */ new Set([...context.dataset.allColumns, ...headers])];
  let invalidHeaders = [];
  for (const header of headers) {
    if (!context.validColumns.includes(header)) {
      invalidHeaders = [...invalidHeaders, header];
    }
  }
  if (invalidHeaders.length != 0) {
    context.issues.addSchemaIssue("CsvColumnMissingFromMetadata", [
      {
        ...context.file,
        evidence: `Column headers: [${invalidHeaders}] do not appear in variableMeasured. ${schemaPath}`
      }
    ]);
  }
  const schemaOrgIssues = {
    "termIssues": [],
    "unknownNamespaceIssues": [],
    "typeIssues": [],
    "typeMissingIssues": []
  };
  schemaCheck(context, schema, schemaOrgIssues);
}
function evalJsonCheck(rule, context, _schema, schemaPath) {
  const issueKeys = [];
  for (const [key, requirement] of Object.entries(rule.fields)) {
    const severity = getFieldSeverity(requirement, context);
    const keyName = `http://schema.org/${key}`;
    if (severity && severity !== "ignore" && !(keyName in context.expandedSidecar)) {
      if (requirement.issue?.code && requirement.issue?.message) {
        context.issues.add({
          key: requirement.issue.code,
          reason: requirement.issue.message,
          severity,
          files: [{ ...context.file }]
        });
      } else {
        issueKeys.push(key);
      }
    }
  }
  if (issueKeys.length != 0) {
    context.issues.addSchemaIssue("JsonKeyRequired", [
      {
        ...context.file,
        evidence: `metadata object missing fields: [${issueKeys}] as per ${schemaPath}. 
                    If these fields appear to be present in your metadata, then there may be an issue with your schema.org context`
      }
    ]);
  }
}
function schemaCheck(context, schema, issues) {
  const schemaNamespace = "http://schema.org/";
  if ("@type" in context.expandedSidecar) {
    if (context.expandedSidecar["@type"][0] !== `${schemaNamespace}Dataset`) {
      let issueFile2;
      if (Object.keys(context.metadataProvenance).includes("@type")) {
        issueFile2 = context.metadataProvenance["@type"];
      } else {
        issueFile2 = context.dataset.metadataFile;
      }
      context.issues.addSchemaIssue("IncorrectDatasetType", [
        {
          ...issueFile2,
          evidence: `dataset_description.json's "@type" property must have "Dataset" as its value.
                      additionally, the term "Dataset" must implicitly or explicitly use the schema.org namespace.
                      The schema.org namespace can be explicitly set using the "@context" key`
        }
      ]);
      return;
    }
  } else {
    context.issues.addSchemaIssue("MissingDatasetType", [
      {
        ...context.file,
        evidence: `dataset_description.json must have either the "@type" or the "type" property.`
      }
    ]);
    return;
  }
  issues = _schemaCheck(
    context.expandedSidecar,
    context,
    schema,
    "",
    schemaNamespace,
    issues
  );
  logSchemaIssues(context, issues);
}
function logSchemaIssues(context, issues) {
  if (issues.termIssues.length != 0) {
    issues.termIssues.forEach((issue) => {
      const rootKey = issue.split(".")[1];
      let issueFile2;
      if (Object.keys(context.metadataProvenance).includes(rootKey)) {
        issueFile2 = context.metadataProvenance[rootKey];
      } else {
        issueFile2 = context.dataset.metadataFile;
      }
      context.issues.addSchemaIssue("InvalidSchemaorgProperty", [
        {
          ...issueFile2,
          evidence: `This file contains one or more keys that use the schema.org namespace, but are not  official schema.org properties.
                      According to the psych-DS specification, this is not an error, but be advised that these terms will not be
                      machine-interpretable and do not function as linked data elements. These are the keys in question: [${issues.termIssues}]`
        }
      ]);
    });
  }
  if (issues.typeIssues.length != 0) {
    issues.typeIssues.forEach((issue) => {
      const rootKey = issue.split(".")[1];
      let issueFile2;
      if (rootKey in context.metadataProvenance) {
        issueFile2 = context.metadataProvenance[rootKey];
      } else {
        issueFile2 = context.dataset.metadataFile;
      }
      context.issues.addSchemaIssue("InvalidObjectType", [
        {
          ...issueFile2,
          evidence: `This file contains one or more objects with types that do not match the selectional constraints of their keys.
                        Each schema.org property (which take the form of keys in your metadata json) has a specific range of types
                        that can be used as its value. Type constraints for a given property can be found by visiting their corresponding schema.org
                        URL. All properties can take strings or URLS as objects, under the assumption that the string/URL represents a unique ID.
                        Type selection errors occurred at the following locations in your json structure: [${issues.typeIssues}]`
        }
      ]);
    });
  }
  if (issues.typeMissingIssues.length != 0) {
    issues.typeMissingIssues.forEach((issue) => {
      const rootKey = issue.split(".")[1];
      let issueFile2;
      if (Object.keys(context.metadataProvenance).includes(rootKey)) {
        issueFile2 = context.metadataProvenance[rootKey];
      } else {
        issueFile2 = context.dataset.metadataFile;
      }
      context.issues.addSchemaIssue("ObjectTypeMissing", [
        {
          ...issueFile2,
          evidence: `This file contains one or more objects without a @type property. Make sure that any object that you include
                      as the value of a schema.org property contains a valid schema.org @type, unless it is functioning as some kind of 
                      base type, such as Text or URL, containing a @value key. @type is optional, but not required on such objects.
                      The following objects without @type were found: [${issues.typeMissingIssues}]`
        }
      ]);
    });
  }
  if (issues.unknownNamespaceIssues.length != 0) {
    issues.unknownNamespaceIssues.forEach((issue) => {
      const rootKey = issue.split(".")[0];
      let issueFile2;
      if (Object.keys(context.metadataProvenance).includes(rootKey)) {
        issueFile2 = context.metadataProvenance[rootKey];
      } else {
        issueFile2 = context.dataset.metadataFile;
      }
      context.issues.addSchemaIssue("UnknownNamespace", [
        {
          ...issueFile2,
          evidence: `This file contains one or more references to namespaces other than https://schema.org:
                      [${issues.unknownNamespaceIssues}].`
        }
      ]);
    });
  }
}
function _schemaCheck(node, context, schema, objectPath, nameSpace, issues) {
  let superClassSlots = [];
  let thisType = "";
  if ("@type" in node) {
    thisType = node["@type"][0];
    superClassSlots = getSuperClassSlots(
      thisType,
      schema,
      nameSpace
    );
  }
  for (const [key, value] of Object.entries(node)) {
    if (key.startsWith("@")) {
      continue;
    } else {
      if (!key.startsWith(nameSpace)) {
        issues.unknownNamespaceIssues.push(key);
        continue;
      } else {
        const property = key.replace(nameSpace, "");
        let range = [];
        if (property in schema[`schemaOrg.slots`]) {
          if ("range" in schema[`schemaOrg.slots.${property}`]) {
            range.push(schema[`schemaOrg.slots.${property}.range`]);
            range = range.concat(
              getSubClassSlots(
                schema[`schemaOrg.slots.${property}.range`],
                schema,
                nameSpace
              )
            );
          }
          if ("any_of" in schema[`schemaOrg.slots.${property}`]) {
            for (const ran of schema[`schemaOrg.slots.${property}`].any_of) {
              if ("range" in ran) {
                range.push(ran.range);
                range = range.concat(
                  getSubClassSlots(ran.range, schema, nameSpace)
                );
              }
            }
          }
        }
        let subKeys = [];
        if (!superClassSlots.includes(property)) {
          issues.termIssues.push(`${objectPath}.${property}`);
        } else {
          for (let i = 0; i < value.length; i++) {
            const obj = value[i];
            subKeys = Object.keys(obj);
            if (!(subKeys.length === 1 && (subKeys.includes("@id") || subKeys.includes("@value")))) {
              if (subKeys.includes("@type")) {
                const objType = Array.isArray(obj["@type"]) ? obj["@type"][0].replace(nameSpace, "") : obj["@type"].replace(nameSpace, "");
                if (![...range, "Text", "URL"].includes(objType)) {
                  issues.typeIssues.push(
                    `${objectPath}.${property}${i === 0 ? "" : `[${i}]`}`
                  );
                }
                issues = _schemaCheck(
                  obj,
                  context,
                  schema,
                  `${objectPath}.${property}`,
                  nameSpace,
                  issues
                );
              } else {
                issues.typeMissingIssues.push(
                  `${objectPath}.${property}${i === 0 ? "" : `[${i}]`}`
                );
              }
            }
          }
        }
      }
    }
  }
  return issues;
}
function getSuperClassSlots(type, schema, nameSpace) {
  type = type.replace(nameSpace, "");
  if (!(type in schema["schemaOrg.classes"])) {
    return [];
  }
  const slots = schema[`schemaOrg.classes.${type}.slots`] || [];
  const is_a = "is_a" in schema[`schemaOrg.classes.${type}`] ? getSuperClassSlots(schema[`schemaOrg.classes.${type}.is_a`], schema, nameSpace) : [];
  return [...slots, ...is_a];
}
function getSubClassSlots(type, schema, nameSpace) {
  const subClasses = [];
  if (type.includes(nameSpace)) {
    type = type.replace(nameSpace, "");
  }
  if (type in schema[`schemaOrg.classes`]) {
    for (const [key, value] of Object.entries(schema["schemaOrg.classes"])) {
      if ("is_a" in value && value["is_a"] === type) {
        subClasses.push(key);
        subClasses.concat(getSubClassSlots(key, schema, nameSpace));
      }
    }
    return subClasses;
  } else {
    return [];
  }
}
function getFieldSeverity(requirement, context) {
  const levelToSeverity = {
    recommended: "ignore",
    required: "error",
    optional: "ignore",
    prohibited: "ignore"
  };
  let severity = "ignore";
  if (typeof requirement === "string" && requirement in levelToSeverity) {
    severity = levelToSeverity[requirement];
  } else if (typeof requirement === "object" && requirement.level) {
    severity = levelToSeverity[requirement.level];
    const addendumRegex = /(required|recommended) if \`(\w+)\` is \`(\w+)\`/;
    if (requirement.level_addendum) {
      const match = addendumRegex.exec(requirement.level_addendum);
      if (match && match.length === 4) {
        const [_, addendumLevel, key, value] = match;
        if (key in context.sidecar && context.sidecar[key] === value) {
          severity = levelToSeverity[addendumLevel];
        }
      }
    }
  }
  return severity;
}

// src/summary/summary.ts
var Summary = class {
  /**
   * Initializes a new Summary instance with default values
   */
  constructor() {
    this.dataProcessed = false;
    this.totalFiles = -1;
    this.size = 0;
    this.dataTypes = /* @__PURE__ */ new Set();
    this.schemaVersion = "";
    this.suggestedColumns = [];
  }
  /**
   * Updates summary information with data from a new context
   * Tracks file counts, sizes, and data types
   *
   * @param context - Current validation context to process
   * @returns Promise that resolves when update is complete
   */
  async update(context) {
    if (context.file.path.startsWith("/derivatives") && !this.dataProcessed) {
      return;
    }
    this.totalFiles++;
    this.size += await context.file.size;
    if (context.datatype.length) {
      this.dataTypes.add(context.datatype);
    }
  }
  /**
   * Formats the summary data for output
   * Converts internal state to a standardized output format
   *
   * @returns Formatted summary data object
   */
  formatOutput() {
    return {
      totalFiles: this.totalFiles,
      size: this.size,
      dataProcessed: this.dataProcessed,
      dataTypes: Array.from(this.dataTypes),
      schemaVersion: this.schemaVersion,
      suggestedColumns: this.suggestedColumns
    };
  }
};

// src/utils/objectPathHandler.ts
var hasProp = (obj, prop) => {
  return Object.prototype.hasOwnProperty.call(obj, prop);
};
var objectPathHandler = {
  get(target, property) {
    let res = target;
    for (const prop of property.split(".")) {
      if (hasProp(res, prop)) {
        res = res[prop];
      } else {
        return void 0;
      }
    }
    return res;
  }
};

// src/setup/loadSchema.ts
var SCHEMA_BASE_URL = "https://raw.githubusercontent.com/psych-ds/psych-DS/master/schema_model/versions/jsons";
var SCHEMA_ORG_URL = "https://raw.githubusercontent.com/psych-ds/psych-DS/master/schema_model/external_schemas/schemaorg/schemaorg.json";
var defaultSchema = {};
var defaultSchemaOrg = {};
async function loadDefaultSchemas() {
  try {
    if (isBrowser) {
      defaultSchema = await fetchJSON("/defaultSchema.json") || {};
      defaultSchemaOrg = await fetchJSON("/defaultSchemaOrg.json") || {};
    } else {
      const dirname = getDirname();
      defaultSchema = JSON.parse(
        await readFile(path.join(dirname, "defaultSchema.json"))
      );
      defaultSchemaOrg = JSON.parse(
        await readFile(path.join(dirname, "defaultSchemaOrg.json"))
      );
    }
  } catch (error2) {
    console.error("Error loading default schemas:", error2);
    defaultSchema = {};
    defaultSchemaOrg = {};
  }
}
function getDirname() {
  if (isNode && typeof __dirname !== "undefined") {
    return __dirname;
  } else if (isDeno || isNode && typeof __dirname === "undefined") {
    const url = new URL(import.meta.url);
    return path.dirname(url.pathname);
  } else {
    console.warn("Unable to determine directory in browser environment");
    return "";
  }
}
async function fetchJSON(url) {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
  } catch (error2) {
    console.error(`Error fetching JSON from ${url}:`, error2);
    return null;
  }
}
async function loadSchema(version = "latest") {
  if (Object.keys(defaultSchema).length === 0 || Object.keys(defaultSchemaOrg).length === 0) {
    await loadDefaultSchemas();
  }
  const versionRegex = /^\d+\.\d+\.\d+$/;
  if (version !== "latest" && !versionRegex.test(version)) {
    throw new Error(
      `Invalid version format. Please use 'latest' or 'X.Y.Z' format (e.g., '1.0.0').`
    );
  }
  const schemaUrl = `${SCHEMA_BASE_URL}/${version}/schema.json`;
  let schemaModule;
  let schemaOrgModule;
  try {
    schemaModule = await fetchJSON(schemaUrl);
    schemaOrgModule = await fetchJSON(`${SCHEMA_ORG_URL}?v=${Date.now()}`);
    if (!schemaModule) {
      console.warn(
        `Failed to fetch schema from ${schemaUrl}, using default schema`
      );
      schemaModule = defaultSchema;
    }
    if (!schemaOrgModule) {
      console.warn(`Failed to fetch schemaOrg, using default schemaOrg`);
      schemaOrgModule = defaultSchemaOrg;
    }
    const combinedSchema = {
      ...schemaModule,
      schemaOrg: schemaOrgModule
    };
    return new Proxy(combinedSchema, objectPathHandler);
  } catch (error2) {
    console.error(`Error loading schema: ${error2}`);
    console.warn("Falling back to default schema");
    return new Proxy(
      { ...defaultSchema, schemaOrg: defaultSchemaOrg },
      objectPathHandler
    );
  }
}

// src/types/columns.ts
var ColumnsMap = class extends Map {
  constructor() {
    super();
    const columns = /* @__PURE__ */ new Map();
    return columns;
  }
};

// src/schema/elements.ts
function _readElements(filename) {
  let extension = "";
  let suffix = "";
  const keywords = {};
  const parts = filename.split("_");
  for (let i = 0; i < parts.length - 1; i++) {
    const [key, value] = parts[i].split("-");
    keywords[key] = value || "NOKEYWORD";
  }
  const lastPart = parts[parts.length - 1];
  const extStart = lastPart.indexOf(".");
  if (extStart === -1) {
    suffix = lastPart;
  } else {
    suffix = lastPart.slice(0, extStart);
    extension = lastPart.slice(extStart);
  }
  return { keywords, suffix, extension };
}
var readElements = memoize(_readElements);

// src/files/csv.ts
var normalizeEOL = (str) => str.replace(/\r\n/g, "\n").replace(/\r/g, "\n");
async function parseCSV(contents) {
  const columns = new ColumnsMap();
  const issues = [];
  const normalizedStr = normalizeEOL(contents);
  try {
    const parse3 = isBrowser ? (await Promise.resolve().then(() => (init_sync(), sync_exports))).parse : (await Promise.resolve().then(() => (init_sync2(), sync_exports2))).parse;
    const rows = parse3(normalizedStr, {
      skip_empty_lines: false,
      relax_column_count: true
      // Allow inconsistent columns for better error handling
    });
    const headers = rows.length ? rows[0] : [];
    if (new Set(headers).size !== headers.length) {
      issues.push({ "issue": "CSVHeaderRepeated", "message": null });
    }
    if (headers.length === 0) {
      issues.push({ "issue": "CSVHeaderMissing", "message": null });
    } else {
      headers.forEach((x) => {
        columns[x] = [];
      });
      for (let i = 1; i < rows.length; i++) {
        if (rows[i].length !== headers.length) {
          issues.push({
            "issue": "CSVHeaderLengthMismatch",
            "message": `Row ${i + 1} has ${rows[i].length} columns, expected ${headers.length}`
          });
        } else {
          for (let j = 0; j < headers.length; j++) {
            const col = columns[headers[j]];
            col.push(rows[i][j]);
          }
        }
      }
      if (columns["row_id"] && Array.isArray(columns["row_id"])) {
        const rowIdSet = new Set(columns["row_id"]);
        if (rowIdSet.size !== columns["row_id"].length) {
          issues.push({ "issue": "RowidValuesNotUnique", "message": null });
        }
      }
    }
  } catch (error2) {
    issues.push({
      "issue": "CSVFormattingError",
      // deno-lint-ignore no-explicit-any
      "message": error2.message
    });
  }
  return {
    "columns": columns,
    "issues": issues
  };
}

// src/schema/context.ts
var psychDSContextDataset = class {
  /**
   * Creates a new dataset context
   * @param options - Validator configuration options
   * @param metadataFile - Dataset metadata file
   * @param description - Dataset description object
   */
  constructor(options, metadataFile, description = {}) {
    this.dataset_description = description;
    this.files = [];
    this.metadataFile = metadataFile;
    this.baseDirs = [];
    this.sidecarCache = {};
    this.tree = {};
    this.ignored = [];
    this.allColumns = [];
    if (options) {
      this.options = options;
    }
  }
};
var defaultDsContext = new psychDSContextDataset();
var psychDSContext = class {
  /**
   * Creates a new file context
   * @param fileTree - File tree containing the file
   * @param file - File to validate
   * @param issues - Issue collection for tracking problems
   * @param dsContext - Optional dataset context
   */
  constructor(fileTree, file, issues, dsContext) {
    this.fileTree = fileTree;
    this.filenameRules = [];
    this.issues = issues;
    this.file = file;
    this.fileName = file.name.split(".")[0];
    this.baseDir = file.path.split("/").length > 2 ? file.path.split("/")[1] : "/";
    const elements = readElements(file.name);
    this.keywords = elements.keywords;
    this.extension = elements.extension;
    this.suffix = elements.suffix;
    this.dataset = dsContext ? dsContext : defaultDsContext;
    this.datatype = "";
    this.sidecar = dsContext ? dsContext.dataset_description : {};
    this.expandedSidecar = {};
    this.validColumns = [];
    this.metadataProvenance = {};
    this.columns = new ColumnsMap();
    this.suggestedColumns = [];
  }
  /** Gets the file's path */
  get path() {
    return this.file.path;
  }
  /** Gets the dataset root path */
  get datasetPath() {
    return this.fileTree.path;
  }
  /**
   * Loads and processes sidecar metadata files
   * Crawls file tree from root to current file, loading JSON sidecars
   * @param fileTree - Optional specific file tree to process
   */
  async loadSidecar(fileTree) {
    if (!fileTree) {
      fileTree = this.fileTree;
    }
    const validSidecars = fileTree.files.filter((file) => {
      const { suffix, extension } = readElements(file.name);
      return extension === ".json" && suffix === "data" && file.name.split(".")[0] === this.fileName || extension === ".json" && file.name.split(".")[0] == "file_metadata";
    });
    if (validSidecars.length > 1) {
      const exactMatch = validSidecars.find(
        (sidecar) => sidecar.path == this.file.path.replace(this.extension, ".json")
      );
      if (exactMatch) {
        validSidecars.splice(1);
        validSidecars[0] = exactMatch;
      }
    }
    if (validSidecars.length === 1) {
      const validSidecarJson = await validSidecars[0].text().then(JSON.parse);
      this.sidecar = { ...this.sidecar, ...validSidecarJson };
      Object.keys(validSidecarJson).forEach((key) => {
        const baseKey = key.split("/").at(-1);
        this.metadataProvenance[baseKey] = validSidecars[0];
      });
    }
    const nextDir = fileTree.directories.find((directory) => {
      return this.file.path.startsWith(directory.path);
    });
    if (nextDir) {
      await this.loadSidecar(nextDir);
    } else {
      const jsonString = JSON.stringify(this.sidecar);
      if (jsonString in this.dataset.sidecarCache) {
        this.expandedSidecar = this.dataset.sidecarCache[jsonString];
      } else {
        this.expandedSidecar = await this.getExpandedSidecar();
        this.dataset.sidecarCache[jsonString] = this.expandedSidecar;
      }
      this.loadValidColumns();
    }
  }
  /**
   * Extracts valid column names from metadata
   * Used for CSV header validation
   */
  loadValidColumns() {
    if (this.extension !== ".csv") {
      return;
    }
    const nameSpace = "http://schema.org/";
    if (!(`${nameSpace}variableMeasured` in this.expandedSidecar)) {
      return;
    }
    let validColumns = [];
    for (const variable of this.expandedSidecar[`${nameSpace}variableMeasured`]) {
      if ("@value" in variable) {
        validColumns = [...validColumns, variable["@value"]];
      } else {
        if (`${nameSpace}name` in variable) {
          const subVar = variable[`${nameSpace}name`][0];
          if ("@value" in subVar) {
            validColumns = [...validColumns, subVar["@value"]];
          }
        }
      }
    }
    this.validColumns = validColumns;
  }
  /**
   * Loads and validates CSV column data
   */
  async loadColumns() {
    if (this.extension !== ".csv") {
      return;
    }
    let result;
    try {
      result = await parseCSV(await this.file.text());
    } catch (_error) {
      result = /* @__PURE__ */ new Map();
    }
    this.columns = result["columns"];
    this.reportCSVIssues(result["issues"]);
    return;
  }
  /**
   * Records CSV validation issues
   * @param issues - Array of CSV validation issues
   */
  reportCSVIssues(issues) {
    issues.forEach((issue) => {
      if (issue.message) {
        this.issues.addSchemaIssue(
          issue.issue,
          [{ ...this.file, evidence: issue.message }]
        );
      } else {
        this.issues.addSchemaIssue(
          issue.issue,
          [this.file]
        );
      }
    });
  }
  /**
   * Expands JSON-LD metadata with full context
   * Handles cross-platform JSON-LD processing
   * @returns Expanded JSON-LD document
   */
  async getExpandedSidecar() {
    let jsonld;
    if (!isBrowser) {
      const jsonldModule = await import("jsonld");
      jsonld = jsonldModule.default;
    }
    const jsonldToUse = isBrowser ? window.jsonld : jsonld;
    const customDocumentLoader = async (url) => {
      if (url.startsWith("http://schema.org/") || url.startsWith("https://schema.org/")) {
        const safeSchemaUrl = "https://schema.org/version/latest/schemaorg-current-https.jsonld";
        try {
          const response = await fetch(safeSchemaUrl);
          const context = await response.json();
          return {
            contextUrl: null,
            document: context,
            documentUrl: url
          };
        } catch (_error) {
          if (isBrowser) {
            try {
              const context = await fetchJSON("/defaultSchemaOrgJsonLD.json") || {};
              return {
                contextUrl: null,
                document: context,
                documentUrl: url
              };
            } catch (error2) {
              console.log(error2);
            }
          } else {
            const context = JSON.parse(
              await readFile("../setup/defaultSchemaOrgJsonLD.json")
            );
            return {
              contextUrl: null,
              document: context,
              documentUrl: url
            };
          }
        }
      }
      return jsonldToUse.documentLoaders.node()(url);
    };
    try {
      if (!("@context" in this.sidecar) && this.dataset.metadataFile) {
        try {
          this.issues.add({
            key: "INVALID_JSONLD_FORMATTING",
            reason: `Metadata files must follow JSON-LD syntax, which means, among other things, that a @context field must be included.`,
            severity: "error",
            files: [this.dataset.metadataFile]
          });
        } catch (error2) {
          console.log(error2);
        }
        return {};
      }
      const schemaForms = [
        "http://schema.org/",
        "http://schema.org",
        "http://www.schema.org/",
        "http://www.schema.org",
        "https://schema.org/",
        "https://schema.org",
        "https://www.schema.org/",
        "https://www.schema.org/"
      ];
      if ("@context" in this.sidecar && Array.isArray(this.sidecar["@context"]) && schemaForms.includes(this.sidecar["@context"][0])) {
        this.sidecar["@context"] = this.sidecar["@context"][0];
      }
      if ("@context" in this.sidecar && typeof this.sidecar["@context"] == "string" && schemaForms.includes(this.sidecar["@context"])) {
        this.sidecar["@context"] = {
          "@vocab": "http://schema.org/"
        };
      }
      const exp = await jsonldToUse.expand(this.sidecar, {
        documentLoader: customDocumentLoader
      });
      return exp[0] || {};
    } catch (error2) {
      if (error2.details.code.includes("remote context")) {
        error2.message += "\n\n This may be caused by using the wrong URL in the @context field of your dataset_description.json file, or it could be caused by failing to use either http:// or https:// at the beginning.";
      }
      const issueFile2 = {
        ...this.dataset.metadataFile,
        // deno-lint-ignore no-explicit-any
        evidence: error2.message
      };
      this.issues.add({
        key: "INVALID_JSONLD_FORMATTING",
        // deno-lint-ignore no-explicit-any
        reason: `${error2.details.code}`,
        severity: "error",
        files: [issueFile2]
      });
      return {};
    }
  }
  /**
   * Performs all asynchronous loading operations
   * Loads sidecar metadata and CSV columns in parallel
   */
  async asyncLoads() {
    await Promise.allSettled([
      this.loadSidecar(),
      this.loadColumns()
    ]);
  }
};

// src/schema/walk.ts
async function* _walkFileTree(fileTree, root, issues, dsContext) {
  for (const file of fileTree.files) {
    yield new psychDSContext(root, file, issues, dsContext);
  }
  for (const dir of fileTree.directories) {
    if (fileTree.path === "/" && dsContext) {
      dsContext.baseDirs = [...dsContext.baseDirs, `/${dir.name}`];
    }
    yield* _walkFileTree(dir, root, issues, dsContext);
  }
}
async function* walkFileTree(fileTree, issues, dsContext) {
  yield* _walkFileTree(fileTree, fileTree, issues, dsContext);
}

// src/validators/psychds.ts
var CHECKS3 = [
  emptyFile,
  filenameIdentify,
  filenameValidate,
  applyRules
];
async function validate(fileTree, options) {
  options.emitter?.emit("start", { success: true });
  const summary = new Summary();
  const schema = await loadSchema(options.schema);
  const issues = new DatasetIssues(schema);
  let totalCsvFiles = 0;
  let processedCsvFiles = 0;
  options.emitter?.emit("build-tree", { success: true });
  summary.schemaVersion = schema.schema_version;
  const ddFile = fileTree.files.find(
    (file) => file.path === "/dataset_description.json"
  );
  let dsContext;
  if (ddFile) {
    options.emitter?.emit("find-metadata", { success: true });
    try {
      const description = await ddFile.text().then(JSON.parse);
      dsContext = new psychDSContextDataset(options, ddFile, description);
    } catch (_error) {
      dsContext = new psychDSContextDataset(options, ddFile);
      issues.addSchemaIssue(
        "InvalidJsonFormatting",
        [{
          ...ddFile,
          evidence: _error.message
        }]
      );
      options.emitter?.emit("metadata-json", {
        success: false,
        issue: issues.get("INVALID_JSON_FORMATTING")
      });
    }
  } else {
    dsContext = new psychDSContextDataset(options);
  }
  const rulesRecord = {};
  findFileRules(schema, rulesRecord);
  const emitCheck = (event_name, issue_keys, progress) => {
    const fails = issue_keys.filter((issue) => issues.hasIssue({ key: issue }));
    const eventData = fails.length > 0 ? { success: false, issue: issues.get(fails[0]), progress } : { success: true, progress };
    options.emitter?.emit(event_name, eventData);
  };
  let validColumns = {};
  for await (const context of walkFileTree(fileTree, issues, dsContext)) {
    if (context.extension === ".csv" && context.suffix === "data") {
      totalCsvFiles++;
    }
  }
  if (totalCsvFiles > 0) {
    options.emitter?.emit("csv-count-total", { total: totalCsvFiles });
  }
  for await (const context of walkFileTree(fileTree, issues, dsContext)) {
    console.log(context.file.path);
    if (dsContext.baseDirs.includes("/data")) {
      options.emitter?.emit("find-data-dir", { success: true });
    }
    if (context.file.issueInfo.length > 0) {
      context.file.issueInfo.forEach((iss) => {
        issues.addSchemaIssue(
          iss.key,
          [{
            ...context.file,
            evidence: iss.evidence ? iss.evidence : ""
          }]
        );
      });
    }
    if (context.file.ignored)
      continue;
    await context.asyncLoads();
    if (context.extension === ".csv") {
      summary.suggestedColumns = [
        .../* @__PURE__ */ new Set([
          ...summary.suggestedColumns,
          ...Object.keys(context.columns)
        ])
      ];
    }
    for (const check of CHECKS3) {
      await check(schema, context);
    }
    for (const rule of context.filenameRules) {
      rulesRecord[rule] = true;
    }
    await summary.update(context);
    if (context.extension === ".csv" && context.suffix === "data") {
      options.emitter?.emit("check-for-csv", { success: true });
      options.emitter?.emit("metadata-utf8", { success: true });
      emitCheck("metadata-json", ["INVALID_JSON_FORMATTING"]);
      emitCheck("metadata-fields", ["JSON_KEY_REQUIRED"]);
      emitCheck("metadata-jsonld", ["INVALID_JSONLD_FORMATTING"]);
      emitCheck("metadata-type", [
        "INCORRECT_DATASET_TYPE",
        "MISSING_DATASET_TYPE"
      ]);
      emitCheck("metadata-schemaorg", [
        "INVALID_SCHEMAORG_PROPERTY",
        "INVALID_OBJECT_TYPE",
        "OBJECT_TYPE_MISSING"
      ]);
      processedCsvFiles++;
      options.emitter?.emit("csv-progress", {
        current: processedCsvFiles,
        total: totalCsvFiles
      });
      const csvProgress = { current: processedCsvFiles, total: totalCsvFiles };
      emitCheck("csv-keywords", ["FILENAME_KEYWORD_FORMATTING_ERROR", "FILENAME_UNOFFICIAL_KEYWORD_ERROR"], csvProgress);
      emitCheck("csv-parse", ["CSV_FORMATTING_ERROR"], csvProgress);
      emitCheck("csv-header", ["CSV_HEADER_MISSING"], csvProgress);
      emitCheck("csv-header-repeat", ["CSV_HEADER_REPEATED"], csvProgress);
      emitCheck("csv-nomismatch", ["CSV_HEADER_LENGTH_MISMATCH"], csvProgress);
      emitCheck("csv-rowid", ["ROWID_VALUES_NOT_UNIQUE"], csvProgress);
    }
    if (context.validColumns.length != 0) {
      context.validColumns.forEach((col) => {
        if (!(col in validColumns))
          validColumns[col] = false;
        if (col in context.columns)
          validColumns[col] = true;
      });
    }
  }
  const extraVars = Object.entries(validColumns).filter(([key, value]) => !value).map(([key]) => key);
  if (extraVars.length != 0) {
    issues.addSchemaIssue("VariableMissingFromCsvColumns", [
      {
        ...dsContext.metadataFile,
        evidence: `One of the metadata files in your dataset (either dataset_description.json or a sidecar file) 
          contains a variable in variableMeasured that does not appear in any CSV column headers. Here are the variables in question: [${extraVars}]`
      }
    ]);
  }
  options.emitter?.emit("metadata-utf8", { success: true });
  emitCheck("metadata-json", ["INVALID_JSON_FORMATTING"]);
  emitCheck("metadata-fields", ["JSON_KEY_REQUIRED"]);
  emitCheck("metadata-jsonld", ["INVALID_JSONLD_FORMATTING"]);
  emitCheck("metadata-type", [
    "INCORRECT_DATASET_TYPE",
    "MISSING_DATASET_TYPE"
  ]);
  emitCheck("metadata-schemaorg", [
    "INVALID_SCHEMAORG_PROPERTY",
    "INVALID_OBJECT_TYPE",
    "OBJECT_TYPE_MISSING"
  ]);
  emitCheck("csv-keywords", [
    "FILENAME_KEYWORD_FORMATTING_ERROR",
    "FILENAME_UNOFFICIAL_KEYWORD_ERROR"
  ]);
  emitCheck("csv-parse", ["CSV_FORMATTING_ERROR"]);
  emitCheck("csv-header", ["CSV_HEADER_MISSING"]);
  emitCheck("csv-header-repeat", ["CSV_HEADER_REPEATED"]);
  emitCheck("csv-nomismatch", ["CSV_HEADER_LENGTH_MISMATCH"]);
  emitCheck("csv-rowid", ["ROWID_VALUES_NOT_UNIQUE"]);
  emitCheck("check-variableMeasured", ["CSV_COLUMN_MISSING_FROM_METADATA", "VARIABLE_MISSING_FROM_CSV_COLUMNS"]);
  checkDirRules(schema, rulesRecord, dsContext.baseDirs);
  checkMissingRules(schema, rulesRecord, issues);
  emitCheck("find-metadata", ["MISSING_DATASET_DESCRIPTION"]);
  emitCheck("find-data-dir", ["MISSING_DATA_DIRECTORY"]);
  emitCheck("check-for-csv", ["MISSING_DATAFILE"]);
  issues.filterIssues(rulesRecord);
  return {
    valid: [...issues.values()].filter((issue) => issue.severity === "error").length === 0,
    issues,
    summary: summary.formatOutput()
  };
}

// src/types/filetree.ts
var FileTree = class {
  constructor(path2, name, parent) {
    this.path = path2;
    this.files = [];
    this.directories = [];
    this.name = name;
    this.parent = parent;
  }
  contains(parts) {
    if (parts.length === 0) {
      return false;
    } else if (parts.length === 1) {
      return this.files.some((x) => x.name === parts[0]);
    } else if (parts.length > 1) {
      const nextDir = this.directories.find((x) => x.name === parts[0]);
      if (nextDir) {
        return nextDir.contains(parts.slice(1, parts.length));
      } else {
        return false;
      }
    } else {
      return false;
    }
  }
};

// src/setup/requestPermissions.ts
var globalRead = { name: "read" };
async function requestPermission(permission) {
  const status = await Deno.permissions.request(permission);
  if (status.state === "granted") {
    return true;
  } else {
    return false;
  }
}
var requestReadPermission = () => requestPermission(globalRead);

// src/deps/ignore.ts
function makeArray(subject) {
  return Array.isArray(subject) ? subject : [subject];
}
var EMPTY = "";
var SPACE = " ";
var ESCAPE = "\\";
var REGEX_TEST_BLANK_LINE = /^\s+$/;
var REGEX_INVALID_TRAILING_BACKSLASH = /(?:[^\\]|^)\\$/;
var REGEX_REPLACE_LEADING_EXCAPED_EXCLAMATION = /^\\!/;
var REGEX_REPLACE_LEADING_EXCAPED_HASH = /^\\#/;
var REGEX_SPLITALL_CRLF = /\r?\n/g;
var REGEX_TEST_INVALID_PATH = /^\.*\/|^\.+$/;
var SLASH = "/";
var TMP_KEY_IGNORE = "node-ignore";
if (typeof Symbol !== "undefined") {
  TMP_KEY_IGNORE = Symbol.for("node-ignore");
}
var KEY_IGNORE = TMP_KEY_IGNORE;
var define = (object, key, value) => Object.defineProperty(object, key, { value });
var REGEX_REGEXP_RANGE = /([0-z])-([0-z])/g;
var RETURN_FALSE = () => false;
var sanitizeRange = (range) => range.replace(
  REGEX_REGEXP_RANGE,
  (match, from2, to) => from2.charCodeAt(0) <= to.charCodeAt(0) ? match : EMPTY
);
var cleanRangeBackSlash = (slashes) => {
  const { length } = slashes;
  return slashes.slice(0, length - length % 2);
};
var REPLACERS = [
  // > Trailing spaces are ignored unless they are quoted with backslash ("\")
  [
    // (a\ ) -> (a )
    // (a  ) -> (a)
    // (a \ ) -> (a  )
    /\\?\s+$/,
    (match) => match.indexOf("\\") === 0 ? SPACE : EMPTY
  ],
  // replace (\ ) with ' '
  [/\\\s/g, () => SPACE],
  // Escape metacharacters
  // which is written down by users but means special for regular expressions.
  // > There are 12 characters with special meanings:
  // > - the backslash \,
  // > - the caret ^,
  // > - the dollar sign $,
  // > - the period or dot .,
  // > - the vertical bar or pipe symbol |,
  // > - the question mark ?,
  // > - the asterisk or star *,
  // > - the plus sign +,
  // > - the opening parenthesis (,
  // > - the closing parenthesis ),
  // > - and the opening square bracket [,
  // > - the opening curly brace {,
  // > These special characters are often called "metacharacters".
  [/[\\$.|*+(){^]/g, (match) => `\\${match}`],
  [
    // > a question mark (?) matches a single character
    /(?!\\)\?/g,
    () => "[^/]"
  ],
  // leading slash
  [
    // > A leading slash matches the beginning of the pathname.
    // > For example, "/*.c" matches "cat-file.c" but not "mozilla-sha1/sha1.c".
    // A leading slash matches the beginning of the pathname
    /^\//,
    () => "^"
  ],
  // replace special metacharacter slash after the leading slash
  [/\//g, () => "\\/"],
  [
    // > A leading "**" followed by a slash means match in all directories.
    // > For example, "**/foo" matches file or directory "foo" anywhere,
    // > the same as pattern "foo".
    // > "**/foo/bar" matches file or directory "bar" anywhere that is directly
    // >   under directory "foo".
    // Notice that the '*'s have been replaced as '\\*'
    /^\^*\\\*\\\*\\\//,
    // '**/foo' <-> 'foo'
    () => "^(?:.*\\/)?"
  ],
  // starting
  [
    // there will be no leading '/'
    //   (which has been replaced by section "leading slash")
    // If starts with '**', adding a '^' to the regular expression also works
    /^(?=[^^])/,
    function startingReplacer() {
      return !/\/(?!$)/.test(this) ? "(?:^|\\/)" : "^";
    }
  ],
  // two globstars
  [
    // Use lookahead assertions so that we could match more than one `'/**'`
    /\\\/\\\*\\\*(?=\\\/|$)/g,
    // Zero, one or several directories
    // should not use '*', or it will be replaced by the next replacer
    // Check if it is not the last `'/**'`
    (_, index, str) => index + 6 < str.length ? "(?:\\/[^\\/]+)*" : "\\/.+"
  ],
  // normal intermediate wildcards
  [
    // Never replace escaped '*'
    // ignore rule '\*' will match the path '*'
    // 'abc.*/' -> go
    // 'abc.*'  -> skip this rule,
    //    coz trailing single wildcard will be handed by [trailing wildcard]
    /(^|[^\\]+)(\\\*)+(?=.+)/g,
    // '*.js' matches '.js'
    // '*.js' doesn't match 'abc'
    (_, p1, p2) => {
      const unescaped = p2.replace(/\\\*/g, "[^\\/]*");
      return p1 + unescaped;
    }
  ],
  [
    // unescape, revert step 3 except for back slash
    // For example, if a user escape a '\\*',
    // after step 3, the result will be '\\\\\\*'
    /\\\\\\(?=[$.|*+(){^])/g,
    () => ESCAPE
  ],
  [
    // '\\\\' -> '\\'
    /\\\\/g,
    () => ESCAPE
  ],
  [
    // > The range notation, e.g. [a-zA-Z],
    // > can be used to match one of the characters in a range.
    // `\` is escaped by step 3
    /(\\)?\[([^\]/]*?)(\\*)($|\])/g,
    (match, leadEscape, range, endEscape, close) => leadEscape === ESCAPE ? `\\[${range}${cleanRangeBackSlash(endEscape)}${close}` : close === "]" ? endEscape.length % 2 === 0 ? `[${sanitizeRange(range)}${endEscape}]` : "[]" : "[]"
  ],
  // ending
  [
    // 'js' will not match 'js.'
    // 'ab' will not match 'abc'
    /(?:[^*])$/,
    // WTF!
    // https://git-scm.com/docs/gitignore
    // changes in [2.22.1](https://git-scm.com/docs/gitignore/2.22.1)
    // which re-fixes #24, #38
    // > If there is a separator at the end of the pattern then the pattern
    // > will only match directories, otherwise the pattern can match both
    // > files and directories.
    // 'js*' will not match 'a.js'
    // 'js/' will not match 'a.js'
    // 'js' will match 'a.js' and 'a.js/'
    (match) => /\/$/.test(match) ? `${match}$` : `${match}(?=$|\\/$)`
  ],
  // trailing wildcard
  [
    /(\^|\\\/)?\\\*$/,
    (_, p1) => {
      const prefix = p1 ? `${p1}[^/]+` : "[^/]*";
      return `${prefix}(?=$|\\/$)`;
    }
  ]
];
var regexCache = /* @__PURE__ */ Object.create(null);
var makeRegex = (pattern, ignoreCase) => {
  let source = regexCache[pattern];
  if (!source) {
    source = REPLACERS.reduce(
      (prev, current) => prev.replace(current[0], current[1].bind(pattern)),
      pattern
    );
    regexCache[pattern] = source;
  }
  return ignoreCase ? new RegExp(source, "i") : new RegExp(source);
};
var isString = (subject) => typeof subject === "string";
var checkPattern = (pattern) => pattern && isString(pattern) && !REGEX_TEST_BLANK_LINE.test(pattern) && !REGEX_INVALID_TRAILING_BACKSLASH.test(pattern) && // > A line starting with # serves as a comment.
pattern.indexOf("#") !== 0;
var splitPattern = (pattern) => pattern.split(REGEX_SPLITALL_CRLF);
var IgnoreRule = class {
  constructor(origin, pattern, negative, regex) {
    this.origin = origin;
    this.pattern = pattern;
    this.negative = negative;
    this.regex = regex;
  }
};
var createRule = (pattern, ignoreCase) => {
  const origin = pattern;
  let negative = false;
  if (pattern.indexOf("!") === 0) {
    negative = true;
    pattern = pattern.substr(1);
  }
  pattern = pattern.replace(REGEX_REPLACE_LEADING_EXCAPED_EXCLAMATION, "!").replace(REGEX_REPLACE_LEADING_EXCAPED_HASH, "#");
  const regex = makeRegex(pattern, ignoreCase);
  return new IgnoreRule(origin, pattern, negative, regex);
};
var throwError = (message, Ctor) => {
  throw new Ctor(message);
};
var checkPath = (path2, originalPath, doThrow) => {
  if (!isString(path2)) {
    return doThrow(
      `path must be a string, but got \`${originalPath}\``,
      TypeError
    );
  }
  if (!path2) {
    return doThrow(`path must not be empty`, TypeError);
  }
  if (checkPath.isNotRelative(path2)) {
    const r = "`path.relative()`d";
    return doThrow(
      `path should be a ${r} string, but got "${originalPath}"`,
      RangeError
    );
  }
  return true;
};
var isNotRelative = (path2) => REGEX_TEST_INVALID_PATH.test(path2);
checkPath.isNotRelative = isNotRelative;
checkPath.convert = (p) => p;
var Ignore = class {
  constructor({
    ignorecase = true,
    ignoreCase = ignorecase,
    allowRelativePaths = false
  } = {}) {
    define(this, KEY_IGNORE, true);
    this._rules = [];
    this._ignoreCase = ignoreCase;
    this._allowRelativePaths = allowRelativePaths;
    this._initCache();
  }
  _initCache() {
    this._ignoreCache = /* @__PURE__ */ Object.create(null);
    this._testCache = /* @__PURE__ */ Object.create(null);
  }
  _addPattern(pattern) {
    if (pattern && pattern[KEY_IGNORE]) {
      this._rules = this._rules.concat(pattern._rules);
      this._added = true;
      return;
    }
    if (checkPattern(pattern)) {
      const rule = createRule(pattern, this._ignoreCase);
      this._added = true;
      this._rules.push(rule);
    }
  }
  // @param {Array<string> | string | Ignore} pattern
  add(pattern) {
    this._added = false;
    makeArray(isString(pattern) ? splitPattern(pattern) : pattern).forEach(
      this._addPattern,
      this
    );
    if (this._added) {
      this._initCache();
    }
    return this;
  }
  // legacy
  addPattern(pattern) {
    return this.add(pattern);
  }
  //          |           ignored : unignored
  // negative |   0:0   |   0:1   |   1:0   |   1:1
  // -------- | ------- | ------- | ------- | --------
  //     0    |  TEST   |  TEST   |  SKIP   |    X
  //     1    |  TESTIF |  SKIP   |  TEST   |    X
  // - SKIP: always skip
  // - TEST: always test
  // - TESTIF: only test if checkUnignored
  // - X: that never happen
  // @param {boolean} whether should check if the path is unignored,
  //   setting `checkUnignored` to `false` could reduce additional
  //   path matching.
  // @returns {TestResult} true if a file is ignored
  _testOne(path2, checkUnignored) {
    let ignored = false;
    let unignored = false;
    this._rules.forEach((rule) => {
      const { negative } = rule;
      if (unignored === negative && ignored !== unignored || negative && !ignored && !unignored && !checkUnignored) {
        return;
      }
      const matched = rule.regex.test(path2);
      if (matched) {
        ignored = !negative;
        unignored = negative;
      }
    });
    return {
      ignored,
      unignored
    };
  }
  // @returns {TestResult}
  _test(originalPath, cache, checkUnignored, slices) {
    const path2 = originalPath && // Supports nullable path
    checkPath.convert(originalPath);
    checkPath(
      path2,
      originalPath,
      this._allowRelativePaths ? RETURN_FALSE : throwError
    );
    return this._t(path2, cache, checkUnignored, slices);
  }
  _t(path2, cache, checkUnignored, slices) {
    if (path2 in cache) {
      return cache[path2];
    }
    if (!slices) {
      slices = path2.split(SLASH);
    }
    slices.pop();
    if (!slices.length) {
      return cache[path2] = this._testOne(path2, checkUnignored);
    }
    const parent = this._t(
      slices.join(SLASH) + SLASH,
      cache,
      checkUnignored,
      slices
    );
    return cache[path2] = parent.ignored ? parent : this._testOne(path2, checkUnignored);
  }
  ignores(path2) {
    return this._test(path2, this._ignoreCache, false).ignored;
  }
  createFilter() {
    return (path2) => !this.ignores(path2);
  }
  filter(paths) {
    return makeArray(paths).filter(this.createFilter());
  }
  // @returns {TestResult}
  test(path2) {
    return this._test(path2, this._testCache, true);
  }
};
var ignore = (options) => new Ignore(options);
var isPathValid = (path2) => checkPath(path2 && checkPath.convert(path2), path2, RETURN_FALSE);
ignore.isPathValid = isPathValid;

// src/files/ignore.ts
async function readPsychDSIgnore(file) {
  const value = await file.text();
  if (value) {
    const lines = value.split("\n");
    return lines;
  } else {
    return [];
  }
}
var defaultIgnores = [
  ".git**",
  "*.DS_Store",
  ".datalad/",
  ".reproman/",
  "sourcedata/",
  "code/",
  "stimuli/",
  "materials/",
  "results/",
  "products/",
  "analysis/",
  "documentation/",
  "log/"
];
var _ignore;
var FileIgnoreRules = class {
  /**
   * Creates new ignore rules manager
   * Initializes with default patterns and custom configuration
   *
   * @param config - Additional ignore patterns to apply
   */
  constructor(config) {
    /** Internal ignore rules implementation */
    __privateAdd(this, _ignore, void 0);
    __privateSet(this, _ignore, ignore({ allowRelativePaths: true }));
    __privateGet(this, _ignore).add(defaultIgnores);
    __privateGet(this, _ignore).add(config);
  }
  /**
   * Adds additional ignore patterns
   * Expands current rule set with new patterns
   *
   * @param config - New patterns to add
   */
  add(config) {
    __privateGet(this, _ignore).add(config);
  }
  /**
   * Tests if a path should be ignored
   * Checks path against all configured ignore patterns
   *
   * @param path - Path to test (relative to dataset root)
   * @returns True if path matches any ignore pattern
   */
  test(path2) {
    return __privateGet(this, _ignore).ignores(path2);
  }
};
_ignore = new WeakMap();

// src/files/deno.ts
var UnicodeDecodeError = class extends Error {
  constructor(message) {
    super(message);
    this.name = "UnicodeDecode";
  }
};
var _ignore2, _fileInfo, _datasetAbsPath, _openHandle, openHandle_fn;
var psychDSFileDeno = class {
  /**
   * Creates a new Deno file instance
   * @param datasetPath - Path to dataset root
   * @param filePath - Relative path to file
   * @param ignore - Ignore rules to apply
   */
  constructor(datasetPath, filePath, ignore2) {
    /**
     * Opens a Deno file handle with read-only access
     * @returns Deno file handle
     * @private
     */
    __privateAdd(this, _openHandle);
    /** File ignore rules */
    __privateAdd(this, _ignore2, void 0);
    /** Cached file information */
    __privateAdd(this, _fileInfo, void 0);
    /** Absolute path to dataset root */
    __privateAdd(this, _datasetAbsPath, void 0);
    __privateSet(this, _datasetAbsPath, datasetPath);
    this.path = filePath;
    this.name = path.basename(filePath);
    this.expanded = {};
    this.issueInfo = [];
    __privateSet(this, _ignore2, ignore2);
    this.webFile = null;
    try {
      __privateSet(this, _fileInfo, Deno.statSync(this._getPath()));
    } catch (error2) {
      if (error2.code === "ENOENT") {
        __privateSet(this, _fileInfo, Deno.lstatSync(this._getPath()));
      }
    }
  }
  /**
   * Gets absolute path to file
   * @returns Absolute file system path
   * @private
   */
  _getPath() {
    return path.join(__privateGet(this, _datasetAbsPath), this.path);
  }
  /** Gets file size in bytes */
  get size() {
    return __privateGet(this, _fileInfo) ? __privateGet(this, _fileInfo).size : -1;
  }
  /** Gets readable stream of file content */
  get stream() {
    return createReadStream(this._getPath());
  }
  /** Checks if file should be ignored */
  get ignored() {
    return __privateGet(this, _ignore2).test(this.path);
  }
  /**
   * Reads entire file as UTF-8 text
   * Checks for and handles UTF-16 encoding detection
   * @returns Promise resolving to file content
   * @throws {UnicodeDecodeError} If file appears to be UTF-16 encoded
   */
  async text() {
    let data;
    if (!isBrowser) {
      data = await readFile(this._getPath());
    } else if (this.webFile) {
      data = await this.webFile.text();
    } else {
      throw new Error("WebFile not set for browser environment");
    }
    if (data.startsWith("\uFFFD")) {
      throw new UnicodeDecodeError("This file appears to be UTF-16");
    }
    return data;
  }
  /**
   * Reads bytes from file in specified range
   * Efficiently handles large files through streaming
   *
   * @param size - Number of bytes to read
   * @param _offset - Start position (currently unused)
   * @returns Promise resolving to byte array
   */
  async readBytes(size, _offset = 0) {
    const stream = this.stream;
    const reader = stream.getReader();
    const result = new Uint8Array(size);
    let bytesRead = 0;
    while (bytesRead < size) {
      const { value, done } = await reader.read();
      if (done)
        break;
      const remaining = size - bytesRead;
      const chunk = value.slice(0, remaining);
      result.set(chunk, bytesRead);
      bytesRead += chunk.length;
    }
    reader.releaseLock();
    return result;
  }
};
_ignore2 = new WeakMap();
_fileInfo = new WeakMap();
_datasetAbsPath = new WeakMap();
_openHandle = new WeakSet();
openHandle_fn = function() {
  const openOptions = { read: true, write: false };
  return Deno.openSync(this._getPath(), openOptions);
};
async function _readFileTree(rootPathOrDict, relativePath, ignore2, parent, context) {
  const name = path.basename(relativePath);
  const tree = new FileTree(relativePath, name, parent);
  if (typeof rootPathOrDict === "string") {
    await requestReadPermission();
    try {
      for await (const dirEntry of Deno.readDir(path.join(rootPathOrDict, relativePath))) {
        if (dirEntry.isFile || dirEntry.isSymlink) {
          const file = new psychDSFileDeno(
            rootPathOrDict,
            path.join(relativePath, dirEntry.name),
            ignore2
          );
          if (dirEntry.name === ".psychds-ignore") {
            ignore2.add(await readPsychDSIgnore(file));
          }
          tree.files.push(file);
        }
        if (dirEntry.isDirectory) {
          const dirTree = await _readFileTree(
            rootPathOrDict,
            path.join(relativePath, dirEntry.name),
            ignore2,
            tree,
            context
          );
          tree.directories.push(dirTree);
        }
      }
    } catch (error2) {
      console.log("The path for your dataset must point to a directory, not a file. Please input a directory for your dataset input.");
    }
  } else {
    for (const key in rootPathOrDict) {
      const path2 = relativePath === "/" ? `/${key}` : `${relativePath}/${key}`;
      if (rootPathOrDict[key]["type"] === "file") {
        const file = new psychDSFileDeno(".", path2, ignore2);
        file.webFile = rootPathOrDict[key]["file"];
        if (key === ".psychds-ignore") {
          ignore2.add(await readPsychDSIgnore(file));
        }
        tree.files.push(file);
      } else {
        const dirTree = await _readFileTree(
          rootPathOrDict[key]["contents"],
          path2,
          ignore2,
          tree,
          context
        );
        tree.directories.push(dirTree);
      }
    }
  }
  return tree;
}
function readFileTree(rootPathOrDict) {
  const ignore2 = new FileIgnoreRules([]);
  return _readFileTree(rootPathOrDict, "/", ignore2);
}

// src/utils/logger.ts
var loggerPromise = (async () => {
  if (isBrowser) {
    return {
      error: console.error,
      warn: console.warn,
      info: console.log,
      debug: console.log,
      checklist: console.log
    };
  } else {
    const winston = await import("winston");
    const { createLogger, format, transports } = winston;
    const winstonLogger = createLogger({
      level: "info",
      levels: {
        error: 0,
        warn: 1,
        info: 2,
        debug: 3,
        checklist: 4
      },
      format: format.combine(
        format.timestamp(),
        // deno-lint-ignore no-explicit-any
        format.printf((info2) => {
          if (info2.level === "checklist") {
            return info2.message;
          }
          return `${info2.timestamp} [${info2.level.toUpperCase()}]: ${info2.message} ${Object.keys(info2).length > 3 ? JSON.stringify(Object.assign({}, info2, {
            timestamp: void 0,
            level: void 0,
            message: void 0
          })) : ""}`;
        })
      ),
      transports: [
        new transports.Console({
          level: "checklist"
        })
      ]
    });
    const logger = {
      error: (message, ...meta) => winstonLogger.error(message, ...meta),
      warn: (message, ...meta) => winstonLogger.warn(message, ...meta),
      info: (message, ...meta) => winstonLogger.info(message, ...meta),
      debug: (message, ...meta) => winstonLogger.debug(message, ...meta),
      checklist: (message) => winstonLogger.log("checklist", message),
      add: (transport) => winstonLogger.add(transport)
    };
    return logger;
  }
})();
var createAsyncLogger = (level) => async (message, ...meta) => {
  const logger = await loggerPromise;
  if (logger[level]) {
    logger[level](message, ...meta);
  }
};
var error = createAsyncLogger("error");
var warn = createAsyncLogger("warn");
var info = createAsyncLogger("info");
var debug = createAsyncLogger("debug");
var checklist = createAsyncLogger("checklist");

// src/utils/output.ts
var chalkInstance = null;
var tableClass = null;
async function importChalk() {
  if (isBrowser)
    return null;
  const chalk = await import("chalk");
  return chalk.default;
}
async function importTable() {
  if (isBrowser)
    return null;
  const cliTable = await import("cli-table3");
  return cliTable.default;
}
var chalkPromise = null;
function getChalk() {
  if (isBrowser) {
    return {
      green: (text) => text,
      red: (text) => text,
      yellow: (text) => text,
      magenta: (text) => text
    };
  }
  if (!chalkPromise) {
    chalkPromise = importChalk();
  }
  return new Proxy({}, {
    get: (_target, prop) => {
      return (text) => {
        if (chalkInstance) {
          return chalkInstance[prop](text);
        }
        return `[CHALK ${prop.toString().toUpperCase()}]${text}[/CHALK]`;
      };
    }
  });
}
function browserFormatIssue(issue, options) {
  const output = [];
  output.push(
    `[${issue.severity.toUpperCase()}] ${issue.reason} (${issue.key})`
  );
  output.push("");
  let fileOutCount = 0;
  issue.files.forEach((file) => {
    if (!options?.verbose && fileOutCount > 2) {
      return;
    }
    output.push(`  .${file.path}`);
    if (file.line) {
      let msg = `    @ line: ${file.line}`;
      if (file.character) {
        msg += ` character: ${file.character}`;
      }
      output.push(msg);
    }
    if (file.evidence) {
      output.push(`    Evidence: ${file.evidence}`);
    }
    fileOutCount++;
  });
  output.push("");
  return output.join("\n");
}
function formatIssue(issue, options) {
  return isBrowser ? browserFormatIssue(issue, options) : nodeFormatIssue(issue, options, getChalk());
}
function nodeFormatIssue(issue, options, chalk) {
  const severity = issue.severity;
  const color = severity === "error" ? chalk.red : chalk.yellow;
  const output = [];
  output.push(
    "	" + color(
      `[${severity.toUpperCase()}] ${issue.reason} (${issue.key})`
    )
  );
  output.push("");
  let fileOutCount = 0;
  issue.files.forEach((file) => {
    if (!options?.verbose && fileOutCount > 2) {
      return;
    }
    output.push("		." + file.path);
    if (file.line) {
      let msg = "			@ line: " + file.line;
      if (file.character) {
        msg += " character: " + file.character;
      }
      output.push(msg);
    }
    if (file.evidence) {
      output.push("			Evidence: " + file.evidence);
    }
    fileOutCount++;
  });
  output.push("");
  return output.join("\n");
}
(async () => {
  if (!isBrowser) {
    chalkInstance = await importChalk();
    tableClass = await importTable();
  }
})();

// src/utils/validationProgressTracker.ts
var ValidationProgressTracker = class {
  /**
   * Creates a new validation progress tracker
   * @param emitter - EventEmitter instance for tracking validation events
   */
  constructor(emitter) {
    this.csvProgress = { current: 0, total: 0 };
    this.emitter = emitter;
    this.steps = [
      {
        key: "start",
        message: {
          imperative: "Start validation",
          pastTense: "Validation started"
        },
        subSteps: []
      },
      {
        key: "check-folder",
        message: {
          imperative: "Find project folder",
          pastTense: "Project folder found"
        },
        subSteps: [
          {
            key: "build-tree",
            message: {
              imperative: "Crawl project folder and construct file tree",
              pastTense: "Project folder crawled and file tree constructed"
            }
          }
        ]
      },
      {
        key: "find-metadata",
        message: {
          imperative: "Find metadata file",
          pastTense: 'Metadata file "dataset_description.json" found in the root folder'
        },
        subSteps: []
      },
      {
        key: "find-data-dir",
        message: {
          imperative: `Find "data" subfolder`,
          pastTense: `"data" subfolder found in the root folder`
        },
        subSteps: []
      },
      {
        key: "parse-metadata",
        message: {
          imperative: 'Parse "dataset_description.json" metadata file',
          pastTense: 'Successfully parsed "dataset_description.json" metadata file'
        },
        subSteps: [
          {
            key: "metadata-utf8",
            message: {
              imperative: "Check metadata file for utf-8 encoding",
              pastTense: "Metadata file is utf-8 encoded"
            }
          },
          {
            key: "metadata-json",
            message: {
              imperative: "Parse metadata file as JSON",
              pastTense: "Metadata file parsed successfully"
            }
          },
          {
            key: "metadata-jsonld",
            message: {
              imperative: "Validate metadata file as JSON-LD",
              pastTense: "Metadata file is valid JSON-LD"
            }
          },
          {
            key: "metadata-fields",
            message: {
              imperative: `Check metadata file for required "name", "description", and "variableMeasured" fields`,
              pastTense: `Metadata file contains required "name", "description", and "variableMeasured" fields.`
            }
          },
          {
            key: "metadata-type",
            message: {
              imperative: 'Check metadata file for field "@type" with value "Dataset"',
              pastTense: 'Metadata file has "@type" field with value "Dataset"'
            }
          }
        ]
      },
      {
        key: "check-for-csv",
        message: {
          imperative: `Check for CSV data files in "data" subfolder`,
          pastTense: `CSV data files found in "data" subfolder`
        },
        subSteps: []
      },
      {
        key: "validate-csvs",
        message: {
          imperative: `Check that all CSV data files are valid`,
          pastTense: `All CSV data files are valid`
        },
        subSteps: [
          {
            key: "csv-keywords",
            message: {
              imperative: `Check filename for keyword formatting `,
              pastTense: `Filename uses valid keyword formatting`
            }
          },
          {
            key: "csv-parse",
            message: {
              imperative: `Parse data file as CSV`,
              pastTense: `Data file successfully parsed as CSV`
            }
          },
          {
            key: "csv-header",
            message: {
              imperative: `Check for header line`,
              pastTense: `Header line found`
            }
          },
          {
            key: "csv-header-repeat",
            message: {
              imperative: `Check for redundant column names`,
              pastTense: `No redundant column names found`
            }
          },
          {
            key: "csv-nomismatch",
            message: {
              imperative: `Check all lines for equal number of cells`,
              pastTense: `All lines have equal number of cells`
            }
          },
          {
            key: "csv-rowid",
            message: {
              imperative: `Check for any row_id columns with non-unique values`,
              pastTense: `All row_id columns have unique values`
            }
          }
        ]
      },
      {
        key: "check-variableMeasured",
        message: {
          imperative: `Confirm that all column headers in CSV data files are found in "variableMeasured" metadata field`,
          pastTense: `All column headers in CSV data files were found in "variableMeasured" metadata field`
        },
        subSteps: []
      }
    ];
    this.stepStatus = /* @__PURE__ */ new Map();
    this.initializeStepStatus();
    this.result = null;
    this.lastUpdateTime = 0;
    this.logger = null;
    this.setupListeners();
    this.initLogger();
    this.emitter.on("csv-count-total", (data) => {
      this.csvProgress.total = data.total;
    });
    this.emitter.on("csv-progress", (data) => {
      this.csvProgress = data;
      this.displayChecklist();
    });
  }
  /**
   * Initializes the logger instance and displays initial checklist
   * @private
   */
  async initLogger() {
    this.logger = await loggerPromise;
    this.displayChecklist();
  }
  /**
   * Sets the initial status for all steps and sub-steps to incomplete
   * @private
   */
  initializeStepStatus() {
    this.steps.forEach((superStep) => {
      this.stepStatus.set(superStep.key, { complete: false, success: false });
      superStep.subSteps.forEach((subStep) => {
        this.stepStatus.set(subStep.key, { complete: false, success: false });
      });
    });
  }
  /**
   * Sets up event listeners for all steps and sub-steps
   * Configures handlers to update status when steps complete
   * @private
   */
  setupListeners() {
    this.steps.forEach((superStep) => {
      if (superStep.subSteps.length === 0) {
        this.emitter.once(
          superStep.key,
          (data) => {
            this.updateStepStatus(superStep.key, data, superStep);
          }
        );
      } else {
        superStep.subSteps.forEach((subStep) => {
          this.emitter.once(
            subStep.key,
            (data) => {
              this.updateStepStatus(subStep.key, data, superStep);
            }
          );
        });
      }
    });
  }
  /**
   * Updates the status of a step or sub-step
   * @param stepKey - Identifier of the step to update
   * @param data - New status data for the step
   * @param superStep - Parent step if updating a sub-step
   * @private
   */
  updateStepStatus(stepKey, data, superStep) {
    if (data.progress && stepKey.startsWith("csv-")) {
      this.csvProgress = data.progress;
    }
    this.stepStatus.set(stepKey, {
      complete: true,
      success: data.success,
      issue: data.issue
    });
    if (superStep && superStep.subSteps.length > 0) {
      this.updateSuperStepStatus(superStep);
    }
    this.displayChecklist();
    this.lastUpdateTime = Date.now();
  }
  getCsvProgressText() {
    if (this.csvProgress.total === 0)
      return "";
    return ` (${this.csvProgress.current}/${this.csvProgress.total} files checked)`;
  }
  getStepMessage(step, isComplete) {
    const baseMessage = isComplete ? step.message.pastTense : step.message.imperative;
    if (step.key === "validate-csvs") {
      return baseMessage + this.getCsvProgressText();
    }
    return baseMessage;
  }
  /**
   * Updates a parent step's status based on its sub-steps
   * Parent step is complete only when all sub-steps are complete
   * @param superStep - Parent step to update
   * @private
   */
  updateSuperStepStatus(superStep) {
    const allSubStepsComplete = superStep.subSteps.every(
      (subStep) => this.stepStatus.get(subStep.key)?.complete
    );
    const allSubStepsSuccess = superStep.subSteps.every(
      (subStep) => this.stepStatus.get(subStep.key)?.success
    );
    this.stepStatus.set(superStep.key, {
      complete: allSubStepsComplete,
      success: allSubStepsSuccess,
      issue: void 0
    });
  }
  /**
   * Displays the current validation progress as a checklist
   * Shows status of all steps, including success/failure indicators
   * and any validation issues encountered
   * @private
   */
  displayChecklist() {
    if (!this.logger) {
      console.warn("Logger not initialized yet");
      return;
    }
    this.logger.info("\x1Bc");
    const checklistLines = ["Validation Progress:"];
    let prevComplete = true;
    let validationFailed = false;
    let prevFails = false;
    this.steps.forEach((superStep, index) => {
      let thisComplete = true;
      const superStepStatus = this.stepStatus.get(superStep.key);
      if (!superStepStatus?.complete) {
        thisComplete = false;
      }
      const isStepCompleteAndValid = (superStepStatus?.complete ?? false) && prevComplete && !prevFails;
      const superStepMessage = this.getStepMessage(
        superStep,
        isStepCompleteAndValid
      );
      const superStepCheckMark = isStepCompleteAndValid && superStepStatus ? superStepStatus.success ? "\u2713" : "\u2717" : " ";
      if (isStepCompleteAndValid) {
        this.emitter.emit("progress", { step: superStep });
        this.emitter.emit("stepStatusChange", {
          stepStatus: Array.from(this.stepStatus.entries()),
          superStep
        });
      }
      checklistLines.push(
        `[${superStepCheckMark}] ${index + 1}. ${superStepMessage}`
      );
      if (isStepCompleteAndValid && superStepStatus && !superStepStatus.success && superStepStatus.issue) {
        validationFailed = true;
        checklistLines.push(
          `   Issue:
 ${formatIssue(superStepStatus.issue)}`
        );
      }
      if (!(superStepStatus?.success ?? true) && superStep.subSteps.length == 0) {
        prevFails = true;
      }
      let subComplete = true;
      superStep.subSteps.forEach((subStep, subIndex) => {
        const subStepStatus = this.stepStatus.get(subStep.key);
        const isSubStepCompleteAndValid = (subStepStatus?.complete ?? false) && subComplete && !prevFails;
        const subStepMessage = isSubStepCompleteAndValid ? subStep.message.pastTense : subStep.message.imperative;
        const subStepCheckMark = isSubStepCompleteAndValid && subStepStatus ? subStepStatus.success ? "\u2713" : "\u2717" : " ";
        checklistLines.push(
          `  [${subStepCheckMark}] ${index + 1}.${subIndex + 1}. ${subStepMessage}`
        );
        if (isSubStepCompleteAndValid && subStepStatus && !subStepStatus.success && subStepStatus.issue) {
          validationFailed = true;
          checklistLines.push(
            `     Issue:
 ${formatIssue(subStepStatus.issue)}`
          );
        }
        if (isSubStepCompleteAndValid) {
          this.emitter.emit("stepStatusChange", {
            stepStatus: Array.from(this.stepStatus.entries()),
            superStep
          });
        }
        if (!(subStepStatus?.complete ?? false)) {
          subComplete = false;
        }
        if (!(subStepStatus?.success ?? true)) {
          prevFails = true;
        }
      });
      prevComplete = thisComplete;
    });
    this.logger.info(checklistLines.join("\n"));
    if (validationFailed) {
      this.emitter.emit("validation-halted");
    }
    if (this.stepStatus.get("check-variableMeasured")?.success) {
      this.emitter.emit("complete");
    }
  }
  /**
   * Waits for the validation process to complete
   * @returns Promise that resolves when validation finishes or halts
   */
  waitForCompletion() {
    if (this.result) {
      return Promise.resolve();
    }
    return new Promise((resolve) => {
      this.emitter.once("complete", () => {
        resolve();
      });
      this.emitter.once("validation-halted", () => {
        resolve();
      });
    });
  }
};

// src/validate-web.ts
async function validateWeb(fileTree, options) {
  const builtFileTree = await readFileTree(fileTree);
  return validate(builtFileTree, options);
}
export {
  ValidationProgressTracker,
  validateWeb
};
//# sourceMappingURL=psychds-validator.js.map

if (typeof window !== "undefined") { window.psychDSValidator = { validateWeb }; }