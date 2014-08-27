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
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.user.client.ui.Anchor;
import com.google.gwt.user.client.ui.Focusable;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.Container;
import fr.putnami.pwt.core.widget.client.DropdownHeader;
import fr.putnami.pwt.core.widget.client.Icon;
import fr.putnami.pwt.core.widget.client.Nav;
import fr.putnami.pwt.core.widget.client.helper.CompositeFocusHelper;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public abstract class AbstractDropdown extends AbstractPanel implements ClickHandler, KeyPressHandler, Focusable, HasAllFocusHandlers {

	private static final CssStyle STYLE_CARET = new SimpleStyle("caret");

	private final Element caret = Document.get().createSpanElement();
	protected final Anchor anchor = new Anchor(true);
	protected Container menuContainer = new Container(UListElement.TAG);

	private CompositeFocusHelper compositeFocusHelper;

	private String iconType;
	private String name;
	private String label;

	public AbstractDropdown(String tagName) {
		super(tagName);
		anchor.addClickHandler(this);
		anchor.addKeyPressHandler(this);

		anchor.getElement().appendChild(caret);

		StyleUtils.addStyle(caret, STYLE_CARET);

		append(anchor);
		append(menuContainer);

		compositeFocusHelper = CompositeFocusHelper.createFocusHelper(this, anchor);
	}

	public AbstractDropdown(String tagName, String label) {
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

	public String getIconType() {
		return iconType;
	}

	public void setIconType(String iconType) {
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

	public abstract void close();

	public abstract void open();

	public abstract void toggleOpen();

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
}
