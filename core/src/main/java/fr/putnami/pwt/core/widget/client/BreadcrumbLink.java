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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.AnchorElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.LIElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.model.client.base.HasDrawable;
import fr.putnami.pwt.core.widget.client.Nav.LinkStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class BreadcrumbLink extends AbstractWidget implements HasDrawable {

	private String iconType;
	private String label;
	private String link;
	private boolean active;

	public BreadcrumbLink() {
		super(LIElement.TAG);
	}

	public BreadcrumbLink(String label) {
		this();
		this.setLabel(label);
	}

	public BreadcrumbLink(String label, String link) {
		this();
		this.setLabel(label);
		this.setLink(link);
	}

	protected BreadcrumbLink(BreadcrumbLink source) {
		super(source);
		this.setActive(source.active);
		this.setLink(source.link);
		this.iconType = source.iconType;
		this.setLabel(source.label);
	}

	@Override
	public IsWidget cloneWidget() {
		return new BreadcrumbLink(this);
	}

	public String getLabel() {
		return this.label;
	}

	public void setLabel(String label) {
		this.label = label;
		this.redraw();
	}

	public String getIconType() {
		return this.iconType;
	}

	public void setIconType(String iconType) {
		this.iconType = iconType;
		this.redraw();
	}

	public void setActive(boolean active) {
		this.active = active;
		this.redraw();
	}

	public boolean isActive() {
		return this.active;
	}

	public String getLink() {
		return this.link;
	}

	public void setLink(String link) {
		this.link = link;
		this.redraw();
	}

	@Override
	public void redraw() {
		StyleUtils.toggleStyle(this, LinkStyle.ACTIVE, this.active);
		Element elem = getElement();
		elem.removeAllChildren();
		if (!active) {
			elem = Document.get().createAnchorElement();
			getElement().appendChild(elem);
			if (this.link != null) {
				AnchorElement.as(elem).setHref(this.link);
			}
		}

		if (this.label != null) {
			elem.setInnerHTML(this.label);
		}
		if (this.iconType != null) {
			Icon icon = new Icon();
			icon.setType(this.iconType);
			elem.insertFirst(icon.getElement());
		}
	}
}
