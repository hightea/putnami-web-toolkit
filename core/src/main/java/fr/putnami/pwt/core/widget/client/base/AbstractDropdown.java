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

public abstract class AbstractDropdown extends AbstractPanel
	implements ClickHandler, KeyPressHandler, Focusable, HasAllFocusHandlers {

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
		this.anchor.addClickHandler(this);
		this.anchor.addKeyPressHandler(this);

		this.anchor.getElement().appendChild(this.caret);

		StyleUtils.addStyle(this.caret, AbstractDropdown.STYLE_CARET);

		this.append(this.anchor);
		this.append(this.menuContainer);

		this.compositeFocusHelper = CompositeFocusHelper.createFocusHelper(this, this.anchor);
	}

	public AbstractDropdown(String tagName, String label) {
		this(tagName);
		this.setLabel(label);
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
		this.resetInner();
	}

	public String getName() {
		return this.name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIconType() {
		return this.iconType;
	}

	public void setIconType(String iconType) {
		this.iconType = iconType;
		this.resetInner();
	}

	public void addAnchorStyle(CssStyle style) {
		StyleUtils.addStyle(this.anchor, style);
	}

	@Override
	public int getTabIndex() {
		return this.anchor.getTabIndex();
	}

	@Override
	public void setAccessKey(char key) {
		this.anchor.setAccessKey(key);
	}

	@Override
	public void setFocus(boolean focused) {
		this.anchor.setFocus(focused);
	}

	@Override
	public void setTabIndex(int index) {
		this.anchor.setTabIndex(index);
	}

	@Override
	public HandlerRegistration addFocusHandler(FocusHandler handler) {
		return this.compositeFocusHelper.addFocusHandler(handler);
	}

	@Override
	public HandlerRegistration addBlurHandler(BlurHandler handler) {
		return this.compositeFocusHelper.addBlurHandler(handler);
	}

	private void resetInner() {
		this.anchor.getElement().removeAllChildren();
		if (this.iconType != null) {
			Icon icon = new Icon();
			icon.setType(this.iconType);

			this.anchor.getElement().appendChild(icon.getElement());
		}
		if (this.label != null) {
			Text textElem = Document.get().createTextNode(this.label);
			this.anchor.getElement().appendChild(textElem);
		}
		Text spaceElem = Document.get().createTextNode(" ");
		this.anchor.getElement().appendChild(spaceElem);
		this.anchor.getElement().appendChild(this.caret);
	}

	protected Anchor getAnchor() {
		return this.anchor;
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof Nav.IsNavContent || w instanceof DropdownHeader) {
			this.addMenuContent(w);
		}
	}

	public void addMenuContent(IsWidget w) {
		this.menuContainer.append(w);
	}

	@Override
	public void clear() {
		this.menuContainer.clear();
	}

	public abstract void close();

	public abstract void open();

	public abstract void toggleOpen();

	@Override
	public void onClick(ClickEvent event) {
		this.toggleOpen();
	}

	@Override
	public void onKeyPress(KeyPressEvent event) {
		if (event.getCharCode() == KeyCodes.KEY_ENTER) {
			this.toggleOpen();
			event.preventDefault();
			event.stopPropagation();
		}
	}
}
