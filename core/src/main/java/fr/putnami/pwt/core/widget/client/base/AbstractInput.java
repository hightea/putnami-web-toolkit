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
package fr.putnami.pwt.core.widget.client.base;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.DoubleClickEvent;
import com.google.gwt.event.dom.client.DoubleClickHandler;
import com.google.gwt.event.dom.client.DragEndEvent;
import com.google.gwt.event.dom.client.DragEndHandler;
import com.google.gwt.event.dom.client.DragEnterEvent;
import com.google.gwt.event.dom.client.DragEnterHandler;
import com.google.gwt.event.dom.client.DragEvent;
import com.google.gwt.event.dom.client.DragHandler;
import com.google.gwt.event.dom.client.DragLeaveEvent;
import com.google.gwt.event.dom.client.DragLeaveHandler;
import com.google.gwt.event.dom.client.DragOverEvent;
import com.google.gwt.event.dom.client.DragOverHandler;
import com.google.gwt.event.dom.client.DragStartEvent;
import com.google.gwt.event.dom.client.DragStartHandler;
import com.google.gwt.event.dom.client.DropEvent;
import com.google.gwt.event.dom.client.DropHandler;
import com.google.gwt.event.dom.client.FocusEvent;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.GestureChangeEvent;
import com.google.gwt.event.dom.client.GestureChangeHandler;
import com.google.gwt.event.dom.client.GestureEndEvent;
import com.google.gwt.event.dom.client.GestureEndHandler;
import com.google.gwt.event.dom.client.GestureStartEvent;
import com.google.gwt.event.dom.client.GestureStartHandler;
import com.google.gwt.event.dom.client.HasAllDragAndDropHandlers;
import com.google.gwt.event.dom.client.HasAllFocusHandlers;
import com.google.gwt.event.dom.client.HasAllGestureHandlers;
import com.google.gwt.event.dom.client.HasAllKeyHandlers;
import com.google.gwt.event.dom.client.HasAllMouseHandlers;
import com.google.gwt.event.dom.client.HasAllTouchHandlers;
import com.google.gwt.event.dom.client.HasClickHandlers;
import com.google.gwt.event.dom.client.HasDoubleClickHandlers;
import com.google.gwt.event.dom.client.KeyDownEvent;
import com.google.gwt.event.dom.client.KeyDownHandler;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.dom.client.MouseDownEvent;
import com.google.gwt.event.dom.client.MouseDownHandler;
import com.google.gwt.event.dom.client.MouseMoveEvent;
import com.google.gwt.event.dom.client.MouseMoveHandler;
import com.google.gwt.event.dom.client.MouseOutEvent;
import com.google.gwt.event.dom.client.MouseOutHandler;
import com.google.gwt.event.dom.client.MouseOverEvent;
import com.google.gwt.event.dom.client.MouseOverHandler;
import com.google.gwt.event.dom.client.MouseUpEvent;
import com.google.gwt.event.dom.client.MouseUpHandler;
import com.google.gwt.event.dom.client.MouseWheelEvent;
import com.google.gwt.event.dom.client.MouseWheelHandler;
import com.google.gwt.event.dom.client.TouchCancelEvent;
import com.google.gwt.event.dom.client.TouchCancelHandler;
import com.google.gwt.event.dom.client.TouchEndEvent;
import com.google.gwt.event.dom.client.TouchEndHandler;
import com.google.gwt.event.dom.client.TouchMoveEvent;
import com.google.gwt.event.dom.client.TouchMoveHandler;
import com.google.gwt.event.dom.client.TouchStartEvent;
import com.google.gwt.event.dom.client.TouchStartHandler;
import com.google.gwt.event.logical.shared.HasValueChangeHandlers;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.user.client.ui.impl.FocusImpl;
import com.google.web.bindery.event.shared.HandlerRegistration;

import java.util.Collection;
import java.util.Collections;

import fr.putnami.pwt.core.editor.client.EditorInput;
import fr.putnami.pwt.core.editor.client.EditorLeaf;
import fr.putnami.pwt.core.editor.client.Error;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent;
import fr.putnami.pwt.core.editor.client.event.DirtyEvent.Handler;
import fr.putnami.pwt.core.editor.client.util.ValidationUtils;
import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.model.client.base.HasHtmlId;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractInput<I> extends AbstractComposite
	implements DirtyEvent.HasDirtyHandlers, EditorInput<I>, EditorLeaf, HasHtmlId, Focusable, HasValueChangeHandlers<I>,
	HasAllFocusHandlers, HasClickHandlers, HasDoubleClickHandlers, HasAllDragAndDropHandlers, HasAllGestureHandlers,
	HasAllKeyHandlers, HasAllMouseHandlers, HasAllTouchHandlers {

	public static final CssStyle STYLE_CONTROL = new SimpleStyle("form-control");

	private static final FocusImpl FOCUS_IMPL = FocusImpl.getFocusImplForPanel();

	private String path;

	private Collection<Error> errors;
	private Collection<Validator<I>> validators;

	private I value;

	private String htmlId;

	private Character accessKey;
	private Integer tabIndex;

	protected AbstractInput() {
	}

	public AbstractInput(Widget widget) {
		this.initWidget(widget);
	}

	protected AbstractInput(Widget widget, AbstractInput<I> source) {
		super(source);
		this.path = source.path;
		this.htmlId = source.htmlId;
		if (widget != null) {
			this.initWidget(widget);
			this.endCopy(source);
		}
	}

	protected void endCopy(AbstractInput<I> source) {
		if (source.tabIndex != null) {
			this.setTabIndex(source.tabIndex);
		}
		if (source.accessKey != null) {
			this.setAccessKey(source.accessKey);
		}

		if (source.validators != null) {
			for (Validator<I> validator : source.validators) {
				this.addValidator(validator);
			}
		}
	}

	protected void endConstruct() {
		this.setHtmlId(this.htmlId);
	}

	@Override
	protected void initWidget(Widget widget) {
		super.initWidget(widget);
		StyleUtils.addStyle(this, AbstractInput.STYLE_CONTROL);
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public void setHtmlId(String htmlId) {
		this.htmlId = htmlId;
		if (htmlId != null) {
			this.getElement().setId(htmlId);
		}
	}

	@Override
	public String getHtmlId() {
		return this.htmlId;
	}

	@Override
	public HandlerRegistration addDirtyHandler(Handler handler) {
		return EventBus.get().addHandlerToSource(DirtyEvent.TYPE, this, handler);
	}

	@Override
	public I getValue() {
		return this.value;
	}

	public void setValue(I value) {
		this.value = value;
	}

	@Override
	public boolean hasErrors() {
		return this.errors != null && !this.errors.isEmpty();
	}

	@Override
	public Iterable<Error> getErrors() {
		return this.errors == null ? Collections.<Error> emptyList() : Iterables.unmodifiableIterable(this.errors);
	}

	protected void clearErrors() {
		if (this.errors != null) {
			this.errors.clear();
			this.errors = null;
		}
	}

	protected void addError(Error error) {
		if (this.errors == null) {
			this.errors = Lists.newArrayList();
		}
		this.errors.add(error);
	}

	@Override
	public void addValidator(Validator<I> validator) {
		if (this.validators == null) {
			this.validators = Lists.newArrayList();
		}
		this.validators.add(validator);
	}

	protected void validate(I value) {
		Collection<Error> currentErrors = ValidationUtils.validate(this.validators, this, value);
		for (Error error : currentErrors) {
			this.addError(error);
		}
	}

	@Override
	public void setFocus(boolean focused) {
		if (focused) {
			AbstractInput.FOCUS_IMPL.focus(this.getElement());
		} else {
			AbstractInput.FOCUS_IMPL.blur(this.getElement());
		}
	}

	@Override
	public void setTabIndex(int index) {
		this.tabIndex = index;
		AbstractInput.FOCUS_IMPL.setTabIndex(this.getElement(), index);
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addBlurHandler(BlurHandler handler) {
		return this.addDomHandler(handler, BlurEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addClickHandler(ClickHandler handler) {
		return this.addDomHandler(handler, ClickEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDoubleClickHandler(DoubleClickHandler handler) {
		return this.addDomHandler(handler, DoubleClickEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragEndHandler(DragEndHandler handler) {
		return this.addBitlessDomHandler(handler, DragEndEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragEnterHandler(DragEnterHandler handler) {
		return this.addBitlessDomHandler(handler, DragEnterEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragHandler(DragHandler handler) {
		return this.addBitlessDomHandler(handler, DragEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragLeaveHandler(DragLeaveHandler handler) {
		return this.addBitlessDomHandler(handler, DragLeaveEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragOverHandler(DragOverHandler handler) {
		return this.addBitlessDomHandler(handler, DragOverEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDragStartHandler(DragStartHandler handler) {
		return this.addBitlessDomHandler(handler, DragStartEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addDropHandler(DropHandler handler) {
		return this.addBitlessDomHandler(handler, DropEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addFocusHandler(FocusHandler handler) {
		return this.addDomHandler(handler, FocusEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addGestureChangeHandler(GestureChangeHandler handler) {
		return this.addDomHandler(handler, GestureChangeEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addGestureEndHandler(GestureEndHandler handler) {
		return this.addDomHandler(handler, GestureEndEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addGestureStartHandler(GestureStartHandler handler) {
		return this.addDomHandler(handler, GestureStartEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addKeyDownHandler(KeyDownHandler handler) {
		return this.addDomHandler(handler, KeyDownEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addKeyPressHandler(KeyPressHandler handler) {
		return this.addDomHandler(handler, KeyPressEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addKeyUpHandler(KeyUpHandler handler) {
		return this.addDomHandler(handler, KeyUpEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseDownHandler(MouseDownHandler handler) {
		return this.addDomHandler(handler, MouseDownEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseMoveHandler(MouseMoveHandler handler) {
		return this.addDomHandler(handler, MouseMoveEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseOutHandler(MouseOutHandler handler) {
		return this.addDomHandler(handler, MouseOutEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseOverHandler(MouseOverHandler handler) {
		return this.addDomHandler(handler, MouseOverEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseUpHandler(MouseUpHandler handler) {
		return this.addDomHandler(handler, MouseUpEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addMouseWheelHandler(MouseWheelHandler handler) {
		return this.addDomHandler(handler, MouseWheelEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addTouchCancelHandler(TouchCancelHandler handler) {
		return this.addDomHandler(handler, TouchCancelEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addTouchEndHandler(TouchEndHandler handler) {
		return this.addDomHandler(handler, TouchEndEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addTouchMoveHandler(TouchMoveHandler handler) {
		return this.addDomHandler(handler, TouchMoveEvent.getType());
	}

	@Override
	public com.google.gwt.event.shared.HandlerRegistration addTouchStartHandler(TouchStartHandler handler) {
		return this.addDomHandler(handler, TouchStartEvent.getType());
	}

	@Override
	public int getTabIndex() {
		return AbstractInput.FOCUS_IMPL.getTabIndex(this.getElement());
	}

	@Override
	public void setAccessKey(char key) {
		this.accessKey = key;
		AbstractInput.FOCUS_IMPL.setAccessKey(this.getElement(), key);
	}

	public char getAccessKey() {
		return this.accessKey;
	}

}
