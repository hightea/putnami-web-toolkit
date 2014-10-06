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
package fr.putnami.pwt.plugin.code.client.input;

import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.TextArea;

import fr.putnami.pwt.core.model.client.base.HasPlaceholder;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.plugin.code.client.event.LiveValueChangeEvent;
import fr.putnami.pwt.plugin.code.client.event.LiveValueChangeEvent.Handler;

public class CodeInputImpl extends Composite implements CodeInput, ValueChangeHandler<String>,
		KeyUpHandler {

	private static final CssStyle INPUT_STYLE = new SimpleStyle("code-editor-input");

	private TextArea textArea = new TextArea();
	private String placeholder;
	private String currentValue;

	public CodeInputImpl() {
		this.addValueChangeHandler(this);
		this.addKeyUpHandler(this);
		this.initWidget(this.textArea);
		StyleUtils.addStyle(this, CodeInputImpl.INPUT_STYLE);
	}

	@Override
	public HandlerRegistration addValueChangeHandler(ValueChangeHandler<String> handler) {
		return this.textArea.addValueChangeHandler(handler);
	}

	@Override
	public HandlerRegistration addKeyDownHandler(KeyDownHandler handler) {
		return this.textArea.addKeyDownHandler(handler);
	}

	@Override
	public HandlerRegistration addKeyUpHandler(KeyUpHandler handler) {
		return this.textArea.addKeyUpHandler(handler);
	}

	@Override
	public HandlerRegistration addBlurHandler(BlurHandler handler) {
		return this.textArea.addBlurHandler(handler);
	}

	@Override
	public HandlerRegistration addFocusHandler(FocusHandler handler) {
		return this.textArea.addFocusHandler(handler);
	}

	@Override
	public HandlerRegistration addLiveValueChangeHandler(Handler handler) {
		return this.addHandler(handler, LiveValueChangeEvent.TYPE);
	}

	@Override
	public String getText() {
		return this.textArea.getText();
	}

	@Override
	public void setText(String text) {
		this.currentValue = text;
		this.textArea.setValue(text, false);
	}

	@Override
	public int getCursorPosition() {
		return this.textArea.getCursorPos();
	}

	@Override
	public void setCursorPosition(int cursorPosition) {
		this.textArea.setCursorPos(cursorPosition);
	}

	@Override
	public void setFocus(boolean focused) {
		this.textArea.setFocus(focused);
	}

	@Override
	public int getTabIndex() {
		return this.textArea.getTabIndex();
	}

	@Override
	public void setAccessKey(char key) {
		this.textArea.setAccessKey(key);
	}

	@Override
	public void setTabIndex(int index) {
		this.textArea.setTabIndex(index);
	}

	@Override
	public String getPlaceholder() {
		return this.placeholder;
	}

	@Override
	public void setPlaceholder(String placeholder) {
		this.placeholder = placeholder;
		this.textArea.getElement().setAttribute(HasPlaceholder.PLACEHOLDER_ATTRIBUTE, placeholder);
	}

	@Override
	public void onKeyUp(KeyUpEvent event) {
		this.fireLiveValueChangeEvent();
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		this.fireLiveValueChangeEvent();
	}

	private void fireLiveValueChangeEvent() {
		LiveValueChangeEvent.fireIfNotEqual(this, this.currentValue, this.textArea.getText());
		this.currentValue = this.textArea.getText();
	}
}
