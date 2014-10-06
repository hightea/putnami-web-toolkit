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

import com.google.common.collect.Sets;
import com.google.gwt.dom.client.DivElement;
import com.google.gwt.uibinder.client.UiConstructor;
import com.google.gwt.user.client.ui.HasOneWidget;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Collections;
import java.util.Iterator;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorComposite;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.base.AbstractComposite;
import fr.putnami.pwt.core.widget.client.base.BaseSimplePanel;

public class OneWidgetPanel extends AbstractComposite implements
		HasWidgets.ForIsWidget,
		HasOneWidget,
		CloneableWidget,
		EditorComposite {

	private final BaseSimplePanel container;

	private String path;

	public OneWidgetPanel() {
		this(DivElement.TAG);
	}

	@UiConstructor
	public OneWidgetPanel(String tag) {
		container = new BaseSimplePanel(tag);
		endConstruct();
	}

	public OneWidgetPanel(OneWidgetPanel source) {
		super(source);
		container = (BaseSimplePanel) source.container.cloneWidget();
		endConstruct();
	}

	private void endConstruct() {
		initWidget(container);
	}

	@Override
	public IsWidget cloneWidget() {
		return new OneWidgetPanel(this);
	}

	@Override
	public Widget getWidget() {
		return container.getWidget();
	}

	@Override
	public void setWidget(IsWidget w) {
		container.setWidget(w);
	}

	@Override
	public void setWidget(Widget w) {
		setWidget((IsWidget) w);
	}

	/**
	 * Do not call this method. Use {@link OneWidgetPanel#setWidget(IsWidget)} instead.
	 */
	@Override
	public void add(Widget w) {
		add((IsWidget) w);
	}

	/**
	 * Do not call this method. Use {@link OneWidgetPanel#setWidget(IsWidget)} instead.
	 */
	@Override
	public void add(IsWidget w) {
		container.add(w);
	}

	@Override
	public void clear() {
		container.clear();
	}

	@Override
	public Iterator<Widget> iterator() {
		return container.iterator();
	}

	@Override
	public boolean remove(Widget w) {
		return remove((IsWidget) w);
	}

	@Override
	public boolean remove(IsWidget w) {
		return container.remove(w);
	}

	@Override
	public Iterable<Editor> getEditors() {
		if (container.getWidget() instanceof Editor) {
			return Sets.newHashSet((Editor) container.getWidget());
		}
		return Collections.emptySet();
	}

	@Override
	public String getPath() {
		return path == null ? Path.ROOT_PATH : path;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

}
