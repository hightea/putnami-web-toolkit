/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.widget.client;

import com.google.common.base.Objects;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.EventTarget;
import com.google.gwt.dom.client.InputElement;
import com.google.gwt.dom.client.LabelElement;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.DomEvent;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.DOM;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.ui.HasText;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.SimplePanel;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.model.client.base.HasHtmlFor;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractInput;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class InputBoolean extends AbstractInput<Boolean> implements
		HasText,
		HasHtmlFor,
		EditorLabel {

	private static final CssStyle STYLE_CHECKBOX = new SimpleStyle("checkbox");

	private final InputElement checkbocElement = InputElement.as(DOM.createInputCheck());
	private final LabelElement labelElement = Document.get().createLabelElement();

	private String text;

	private HandlerRegistration clickHandlerRegistration;

	private String htmlId;

	public InputBoolean() {
		super(new SimplePanel());
		endConstruct();
		StyleUtils.addStyle(this, STYLE_CHECKBOX);
	}

	protected InputBoolean(InputBoolean source) {
		super(new SimplePanel(), source);
		endConstruct();
		setText(source.text);
	}

	@Override
	protected void endConstruct() {
		getElement().appendChild(labelElement);
		labelElement.appendChild(checkbocElement);
		super.endConstruct();
		StyleUtils.removeStyle(this, STYLE_CONTROL);
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputBoolean(this);
	}

	@Override
	public void setHtmlId(String htmlId) {
		this.htmlId = htmlId;
		checkbocElement.setId(htmlId);
		labelElement.setHtmlFor(htmlId);
	}

	@Override
	public String getHtmlId() {
		return this.htmlId;
	}

	@Override
	public void setHtmlFor(String htmlFor) {
		setHtmlId(htmlFor);
	}

	@Override
	public String getHtmlFor() {
		return getHtmlId();
	}

	@Override
	public boolean isDirty() {
		return !Objects.equal(getValue(), getInputValue());
	}

	private boolean eventTargetsLabelOrChild(DomEvent<?> event) {
		Event nativeEvent = Event.as(event.getNativeEvent());
		EventTarget target = nativeEvent.getEventTarget();
		if (Element.is(target)) {
			return labelElement.isOrHasChild(Element.as(target));
		}
		return false;
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		if (clickHandlerRegistration == null) {
			clickHandlerRegistration = addDomHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					if (!eventTargetsLabelOrChild(event)) {
						setInputValue(!getInputValue());
					}
					DirtyEvent.fire(InputBoolean.this);
					ValueChangeEvent.fire(InputBoolean.this, getInputValue());
				}
			}, ClickEvent.getType());
		}
		return super.addDirtyHandler(handler);
	}

	private Boolean getInputValue() {
		return this.checkbocElement.isChecked();
	}

	private void setInputValue(boolean value) {
		checkbocElement.setChecked(value);
	}

	@Override
	public Boolean flush() {
		Boolean value = getInputValue();
		validate(value);
		if (!hasErrors()) {
			setValue(value);
		}
		return getValue();
	}

	@Override
	public void edit(Boolean value) {
		boolean boolValue = Boolean.TRUE.equals(value);
		setValue(boolValue);
		setInputValue(boolValue);
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public boolean isLabelMandatory() {
		return false;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {
				EditorLabel.CHECKBOX_SUFFIX
		};
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
		labelElement.removeAllChildren();
		labelElement.appendChild(checkbocElement);
		if (text != null) {
			labelElement.appendChild(Document.get().createTextNode(" " + text));
		}
	}

	@Override
	public int getTabIndex() {
		return checkbocElement.getTabIndex();
	}

	@Override
	public void setAccessKey(char key) {
		checkbocElement.setAccessKey(Character.toString(key));
	}

	@Override
	public void setFocus(boolean focused) {
		if (focused) {
			checkbocElement.focus();
		}
		else {
			checkbocElement.blur();
		}
	}

	@Override
	public void setTabIndex(int index) {
		checkbocElement.setTabIndex(index);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addValueChangeHandler(ValueChangeHandler<Boolean> handler) {
		return addHandler(handler, ValueChangeEvent.getType());
	}
}
