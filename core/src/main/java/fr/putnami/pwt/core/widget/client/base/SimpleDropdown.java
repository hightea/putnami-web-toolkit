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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.EventTarget;
import com.google.gwt.dom.client.Text;
import com.google.gwt.dom.client.UListElement;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.FocusHandler;
import com.google.gwt.event.dom.client.HasAllFocusHandlers;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyPressEvent;
import com.google.gwt.event.dom.client.KeyPressHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.Event;
import com.google.gwt.user.client.Event.NativePreviewEvent;
import com.google.gwt.user.client.Event.NativePreviewHandler;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.DropdownHeader;
import fr.putnami.pwt.core.widget.client.Icon;
import fr.putnami.pwt.core.widget.client.Nav;
import fr.putnami.pwt.core.widget.client.constant.IconType;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class SimpleDropdown extends AbstractPanel implements ClickHandler, KeyPressHandler, Focusable, HasAllFocusHandlers {

	private static final CssStyle STYLE_DROPDOWN = new SimpleStyle("dropdown");
	private static final CssStyle STYLE_MENU = new SimpleStyle("dropdown-menu");
	private static final CssStyle STYLE_TOGGLE = new SimpleStyle("dropdown-toggle");
	private static final CssStyle STYLE_CARET = new SimpleStyle("caret");

	private static final CssStyle STYLE_OPEN = new SimpleStyle("open");
	private static final CssStyle STYLE_OPEN_UP = new SimpleStyle("open-up");
	private static final CssStyle STYLE_OPEN_LEFT = new SimpleStyle("open-left");

	private final Anchor anchor = new Anchor(true);
	private final Element caret = Document.get().createSpanElement();

	private AbstractPanel menuContainer = new AbstractPanel(UListElement.TAG) {
	};

	private HandlerRegistration nativePreviewHandlerRegistration;
	private HandlerRegistration historyHandlerRegistration;

	private CompositeFocusHelper compositeFocusHelper;

	private boolean open = false;

	private IconType iconType;
	private String name;
	private String label;

	public SimpleDropdown(String tagName) {
		super(tagName);
		anchor.addClickHandler(this);
		anchor.addKeyPressHandler(this);

		anchor.getElement().appendChild(caret);

		StyleUtils.addStyle(anchor, STYLE_TOGGLE);
		StyleUtils.addStyle(caret, STYLE_CARET);
		StyleUtils.addStyle(menuContainer, STYLE_MENU);

		StyleUtils.addStyle(this, STYLE_DROPDOWN);

		append(anchor);
		append(menuContainer);

		compositeFocusHelper = CompositeFocusHelper.createFocusHelper(this, anchor);
	}

	public SimpleDropdown(String tagName, String label) {
		this(tagName);
		setLabel(label);
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
		resetInner();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public IconType getIconType() {
		return iconType;
	}

	public void setIconType(IconType iconType) {
		this.iconType = iconType;
		resetInner();
	}

	public void addAnchorStyle(CssStyle style) {
		StyleUtils.addStyle(anchor, style);
	}

	@Override
	public int getTabIndex() {
		return anchor.getTabIndex();
	}

	@Override
	public void setAccessKey(char key) {
		anchor.setAccessKey(key);
	}

	@Override
	public void setFocus(boolean focused) {
		anchor.setFocus(focused);
	}

	@Override
	public void setTabIndex(int index) {
		anchor.setTabIndex(index);
	}

	@Override
	public HandlerRegistration addFocusHandler(FocusHandler handler) {
		return compositeFocusHelper.addFocusHandler(handler);
	}

	@Override
	public HandlerRegistration addBlurHandler(BlurHandler handler) {
		return compositeFocusHelper.addBlurHandler(handler);
	}

	private void resetInner() {
		if (iconType == null) {
			iconType = IconType.get(name);
		}

		anchor.getElement().removeAllChildren();
		if (iconType != null) {
			Icon icon = new Icon();
			icon.setType(iconType);

			anchor.getElement().appendChild(icon.getElement());
		}
		if (label != null) {
			Text textElem = Document.get().createTextNode(label);
			anchor.getElement().appendChild(textElem);
		}
		Text spaceElem = Document.get().createTextNode(" ");
		anchor.getElement().appendChild(spaceElem);
		anchor.getElement().appendChild(caret);
	}

	protected Anchor getAnchor() {
		return anchor;
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof Nav.IsNavContent || w instanceof DropdownHeader) {
			addMenuContent(w);
		}
	}

	public void addMenuContent(IsWidget w) {
		menuContainer.append(w);
	}

	@Override
	public void clear() {
		menuContainer.clear();
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

	@Override
	public void onClick(ClickEvent event) {
		toggleOpen();
	};

	@Override
	public void onKeyPress(KeyPressEvent event) {
		if (event.getCharCode() == KeyCodes.KEY_ENTER) {
			toggleOpen();
			event.preventDefault();
			event.stopPropagation();
		}
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

		if (isOpen()) {
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
