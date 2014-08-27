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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.EventTarget;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.Event.NativePreviewEvent;
import com.google.gwt.user.client.Event.NativePreviewHandler;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class SimpleDropdown extends AbstractDropdown {

	private static final CssStyle STYLE_DROPDOWN = new SimpleStyle("dropdown");
	private static final CssStyle STYLE_MENU = new SimpleStyle("dropdown-menu");
	private static final CssStyle STYLE_TOGGLE = new SimpleStyle("dropdown-toggle");

	private static final CssStyle STYLE_OPEN = new SimpleStyle("open");
	private static final CssStyle STYLE_OPEN_UP = new SimpleStyle("open-up");
	private static final CssStyle STYLE_OPEN_LEFT = new SimpleStyle("open-left");

	private HandlerRegistration nativePreviewHandlerRegistration;
	private HandlerRegistration historyHandlerRegistration;

	private boolean open;

	public SimpleDropdown(String tagName) {
		super(tagName);

		StyleUtils.addStyle(anchor, STYLE_TOGGLE);
		StyleUtils.addStyle(menuContainer, STYLE_MENU);

		StyleUtils.addStyle(this, STYLE_DROPDOWN);
	}

	public SimpleDropdown(String tagName, String label) {
		this(tagName);
		setLabel(label);
	}

	public void close() {
		StyleUtils.removeStyle(this, STYLE_OPEN);
		StyleUtils.removeStyle(this, STYLE_OPEN_UP);
		StyleUtils.removeStyle(this, STYLE_OPEN_LEFT);
		open = false;
		updateHandlers();
	}

	public void open() {
		StyleUtils.addStyle(this, STYLE_OPEN);
		Element menuElt = menuContainer.getElement();
		int topMenu = menuElt.getAbsoluteTop() - Window.getScrollTop();
		int menuHeight = menuElt.getOffsetHeight();
		int anchorHeight = anchor.getOffsetHeight();
		int clientHeight = Window.getClientHeight();
		if (topMenu + menuHeight > clientHeight && topMenu >= anchorHeight + menuHeight) {
			StyleUtils.addStyle(this, STYLE_OPEN_UP);
		}
		int leftMenu = menuElt.getAbsoluteLeft() - Window.getScrollLeft();
		int menuWidth = menuElt.getOffsetWidth();
		int anchorWidth = anchor.getOffsetWidth();
		int clientWidth = Window.getClientWidth();
		if (leftMenu + menuWidth > clientWidth && leftMenu >= anchorWidth + menuWidth) {
			StyleUtils.addStyle(this, STYLE_OPEN_LEFT);
		}
		open = true;
		updateHandlers();
	}

	@Override
	public void toggleOpen() {
		if (open) {
			close();
		}
		else {
			open();
		}
		anchor.setFocus(true);
	}

	public boolean isOpen() {
		return open;
	}

	private boolean eventTargetsDropDown(NativePreviewEvent event) {
		Event nativeEvent = Event.as(event.getNativeEvent());
		EventTarget target = nativeEvent.getEventTarget();
		if (Element.is(target)) {
			return getElement().isOrHasChild(Element.as(target));
		}
		return false;
	}

	private void updateHandlers() {
		if (nativePreviewHandlerRegistration != null) {
			nativePreviewHandlerRegistration.removeHandler();
			nativePreviewHandlerRegistration = null;
		}
		if (historyHandlerRegistration != null) {
			historyHandlerRegistration.removeHandler();
			historyHandlerRegistration = null;
		}

		if (open) {
			nativePreviewHandlerRegistration = Event.addNativePreviewHandler(new NativePreviewHandler() {
				@Override
				public void onPreviewNativeEvent(NativePreviewEvent event) {
					if (!eventTargetsDropDown(event)) {
						int type = event.getTypeInt();
						switch (type) {
						case Event.ONMOUSEDOWN:
						case Event.ONTOUCHSTART:
							close();
							break;
						default:
							break;
						}
					}
				}
			});
			historyHandlerRegistration = History.addValueChangeHandler(new ValueChangeHandler<String>() {
				@Override
				public void onValueChange(ValueChangeEvent<String> event) {
					close();
				}
			});
		}
	}
}
