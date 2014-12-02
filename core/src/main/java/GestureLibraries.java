/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package xyz.seto.obscene;

import static xyz.seto.obscene.GestureConstants.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.ref.WeakReference;

public final class GestureLibraries {
    private GestureLibraries() {
    }

    public static GestureLibrary fromFile(String path) {
        return fromFile(new File(path));
    }

    public static GestureLibrary fromFile(File path) {
        return new FileGestureLibrary(path);
    }

    private static class FileGestureLibrary extends GestureLibrary {
        private final File mPath;

        public FileGestureLibrary(File path) {
            mPath = path;
        }

        @Override
        public boolean isReadOnly() {
            return !mPath.canWrite();
        }

        public boolean save() {
            if (!mStore.hasChanged()) return true;

            final File file = mPath;

            final File parentFile = file.getParentFile();
            if (!parentFile.exists()) {
                if (!parentFile.mkdirs()) {
                    return false;
                }
            }

            boolean result = false;
            try {
                //noinspection ResultOfMethodCallIgnored
                file.createNewFile();
                mStore.save(new FileOutputStream(file), true);
                result = true;
            } catch (FileNotFoundException e) {
                // Log.d(LOG_TAG, "Could not save the gesture library in " + mPath, e);
            } catch (IOException e) {
                // Log.d(LOG_TAG, "Could not save the gesture library in " + mPath, e);
            }

            return result;
        }

        public boolean load() {
            boolean result = false;
            final File file = mPath;

            if (file.exists() && file.canRead()) {
                try {
                    mStore.load(new FileInputStream(file), true);
                    result = true;
                } catch (FileNotFoundException e) {
                    // Log.d(LOG_TAG, "Could not load the gesture library from " + mPath, e);
                } catch (IOException e) {
                    // Log.d(LOG_TAG, "Could not load the gesture library from " + mPath, e);
                }
            }

            return result;
        }
    }

}
