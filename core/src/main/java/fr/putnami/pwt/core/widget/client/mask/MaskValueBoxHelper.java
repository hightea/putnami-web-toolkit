/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client.mask;

import com.google.common.collect.Lists;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.dom.client.MouseUpHandler;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.ValueBoxBase;

import java.util.Iterator;
import java.util.List;

public class MaskValueBoxHelper
	implements KeyUpHandler, KeyDownHandler, KeyPressHandler, FocusHandler, BlurHandler, MouseUpHandler {

	public abstract static class TokenHelper extends Timer implements KeyPressHandler, KeyDownHandler {

		protected MaskValueBoxHelper maskHelper;

		protected String placeHolder = "";
		protected boolean optional = false;

		protected String token;

		private int keyDown;

		public void setPlaceHolder(String placeHolder) {
			this.placeHolder = placeHolder;
		}

		public void setToken(String token) {
			this.token = token;
		}

		@Override
		public void run() {
			if (this.internalRun()) {
				this.schedule(this.maskHelper.repeatDelayMilis);
			}
		}

		private boolean internalRun() {

			boolean r = this.handleKeyDown(this.keyDown);
			this.maskHelper.refreshValueBox();
			return r;
		}

		@Override
		public void onKeyPress(KeyPressEvent event) {
			if (this.handleKeyPress(event.getCharCode())) {
				this.maskHelper.refreshValueBox();
				event.preventDefault();
			} else {
				this.maskHelper.focusNext();
				if (this.maskHelper.currentHelper != this) {
					this.maskHelper.currentHelper.onKeyPress(event);
				}
			}
		}

		@Override
		public void onKeyDown(KeyDownEvent event) {
			this.keyDown = event.getNativeKeyCode();
			boolean preventDefault = false;
			switch (event.getNativeKeyCode()) {
				case KeyCodes.KEY_DELETE:
				case KeyCodes.KEY_BACKSPACE:
					if (this.token != null) {
						this.reset();
						this.maskHelper.refreshValueBox();
						event.preventDefault();
					}
					break;
				case KeyCodes.KEY_DOWN:
				case KeyCodes.KEY_UP:
					event.preventDefault();
					break;
				case KeyCodes.KEY_LEFT:
					preventDefault = this.maskHelper.focusPrevious();
					break;
				case KeyCodes.KEY_RIGHT:
					preventDefault = this.maskHelper.focusNext();

					break;
				case KeyCodes.KEY_TAB:
					if (event.isShiftKeyDown()) {
						preventDefault = this.maskHelper.focusPrevious();
					} else {
						preventDefault = this.maskHelper.focusNext();
					}
					break;
				default:
					break;
			}

			if (preventDefault) {
				event.preventDefault();
			}
			if (this.handleKeyDown(this.keyDown)) {
				this.maskHelper.refreshValueBox();
				this.schedule(this.maskHelper.initialDelayMilis);
			}
		}

		public void reset() {
			this.token = null;
			this.cancel();
		}

		protected void focus(boolean forward) {
		}

		protected String flush() {
			return this.token == null ? "" : this.token;
		}

		protected boolean handleKeyDown(int keyDown) {
			return true;
		}

		protected boolean handleKeyPress(char charPressed) {
			return false;
		}
	}

	private final List<TokenHelper> helpers = Lists.newArrayList();
	private final ValueBoxBase<String> valueBox;

	private TokenHelper currentHelper;

	private int initialDelayMilis = 500;
	private int repeatDelayMilis = 50;

	private boolean cursorToReset;

	public MaskValueBoxHelper(ValueBoxBase<String> valueBox) {
		this.valueBox = valueBox;

		valueBox.addKeyDownHandler(this);
		valueBox.addKeyUpHandler(this);
		valueBox.addKeyPressHandler(this);
		valueBox.addBlurHandler(this);
		valueBox.addFocusHandler(this);
		valueBox.addMouseUpHandler(this);
	}

	public void reset() {
		this.helpers.clear();
	}

	public void addTokenHelper(TokenHelper helper) {
		this.helpers.add(helper);
		helper.maskHelper = this;
	}

	@Override
	public void onBlur(BlurEvent event) {
		for (TokenHelper helper : this.helpers) {
			helper.cancel();
		}
	}

	@Override
	public void onFocus(FocusEvent event) {
		this.parseTokens();
		this.cursorToReset = true;
	}

	@Override
	public void onMouseUp(MouseUpEvent event) {
		this.focusOnCursor();
	}

	@Override
	public void onKeyDown(KeyDownEvent event) {
		if (this.cursorToReset) {
			this.focusOnCursor();
			this.cursorToReset = false;
		}
		if (this.currentHelper != null) {
			this.currentHelper.onKeyDown(event);
		}
	}

	@Override
	public void onKeyUp(KeyUpEvent event) {
		if (this.currentHelper != null) {
			this.currentHelper.cancel();
		}
	}

	@Override
	public void onKeyPress(KeyPressEvent event) {
		if (this.currentHelper != null) {
			this.currentHelper.onKeyPress(event);
		}
	}

	private void parseTokens() {
		String value = this.valueBox.getValue();
		for (TokenHelper helper : this.helpers) {
			helper.reset();
		}
		if (value == null) {
			return;
		}
		Iterator<TokenHelper> helperIterator = this.helpers.iterator();
		TokenHelper helper = helperIterator.next();
		for (int i = 0; i < value.length(); i++) {
			char character = value.charAt(i);
			if (!helper.handleKeyPress(character)) {
				while (!helper.handleKeyPress(character) && helperIterator.hasNext()) {
					helper = helperIterator.next();
				}
			}
		}
	}

	private void focusOnCursor() {
		int cursorPosition = this.valueBox.getCursorPos();
		int cnt = 0;
		for (TokenHelper helper : this.helpers) {
			String token = helper.flush();
			int tokenLenght = token != null ? token.length() : 0;
			if (cursorPosition <= cnt + tokenLenght) {
				this.currentHelper = helper;
				this.focus(this.helpers.indexOf(this.currentHelper));
				return;
			}
			cnt += tokenLenght;
		}
		this.focus(0);
	}

	boolean focusNext() {
		if (this.currentHelper == null) {
			return this.focus(0);
		}
		return this.focus(this.helpers.indexOf(this.currentHelper) + 1);
	}

	boolean focusPrevious() {
		if (this.currentHelper == null) {
			return this.focus(0);
		}
		return this.focus(this.helpers.indexOf(this.currentHelper) - 1);
	}

	private boolean focus(int index) {
		for (TokenHelper helper : this.helpers) {
			helper.cancel();
		}
		if (index < 0) {
			this.currentHelper = this.helpers.get(0);
			return false;
		} else if (index + 1 > this.helpers.size()) {
			this.currentHelper = this.helpers.get(this.helpers.size() - 1);
			return false;
		}
		TokenHelper helper = this.helpers.get(index);
		if (this.currentHelper != helper) {
			boolean forward = index > this.helpers.indexOf(this.currentHelper);
			this.currentHelper = helper;
			helper.focus(forward);
			this.highlightHelper();
		}
		return true;
	}

	private void highlightHelper() {
		if (this.currentHelper == null) {
			return;
		}
		int start = 0;
		int helperIndex = this.helpers.indexOf(this.currentHelper);
		for (int i = 0; i <= helperIndex; i++) {
			if (i > 0) {
				start += this.helpers.get(i - 1).flush().length();
			}
		}
		this.valueBox.setSelectionRange(start, this.currentHelper.flush().length());
	}

	private void refreshValueBox() {
		StringBuilder sb = new StringBuilder();
		for (TokenHelper helper : this.helpers) {
			sb.append(helper.flush());
		}
		this.valueBox.setValue(sb.toString());
		this.highlightHelper();
	}

}
